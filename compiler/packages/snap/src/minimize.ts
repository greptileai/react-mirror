/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import type {PluginObj} from '@babel/core';
import {transformFromAstSync} from '@babel/core';
import generate from '@babel/generator';
import traverse from '@babel/traverse';
import * as t from '@babel/types';
import type {parseConfigPragmaForTests as ParseConfigPragma} from 'babel-plugin-react-compiler/src/Utils/TestUtils';
import fs from 'fs';
import path from 'path';
import {parseInput, parseLanguage, parseSourceType} from './compiler.js';
import {PARSE_CONFIG_PRAGMA_IMPORT, PROJECT_SRC} from './constants.js';

type MinimizeOptions = {
  path: string;
};

type CompileSuccess = {kind: 'success'};
type CompileParseError = {kind: 'parse_error'; message: string};
type CompileCompilerError = {
  kind: 'error';
  category: string;
  reason: string;
};
type CompileResult = CompileSuccess | CompileParseError | CompileCompilerError;

/**
 * Compile code and extract error information
 */
function compileAndGetError(
  code: string,
  filename: string,
  language: 'flow' | 'typescript',
  sourceType: 'module' | 'script',
  plugin: PluginObj,
  parseConfigPragmaFn: typeof ParseConfigPragma,
): CompileResult {
  let ast: t.File;
  try {
    ast = parseInput(code, filename, language, sourceType);
  } catch (e: unknown) {
    return {kind: 'parse_error', message: (e as Error).message};
  }

  const firstLine = code.substring(0, code.indexOf('\n'));
  const config = parseConfigPragmaFn(firstLine, {compilationMode: 'all'});
  const options = {
    ...config,
    environment: {
      ...config.environment,
    },
    logger: {
      logEvent: () => {},
      debugLogIRs: () => {},
    },
    enableReanimatedCheck: false,
  };

  try {
    transformFromAstSync(ast, code, {
      filename: '/' + filename,
      highlightCode: false,
      retainLines: true,
      compact: true,
      plugins: [[plugin, options]],
      sourceType: 'module',
      ast: false,
      cloneInputAst: true,
      configFile: false,
      babelrc: false,
    });
    return {kind: 'success'};
  } catch (e: unknown) {
    const error = e as Error & {
      details?: Array<{category: string; reason: string}>;
    };
    // Check if this is a CompilerError with details
    if (error.details && error.details.length > 0) {
      const detail = error.details[0];
      return {
        kind: 'error',
        category: detail.category,
        reason: detail.reason,
      };
    }
    // Fallback for other errors - use error name/message
    return {
      kind: 'error',
      category: error.name ?? 'Error',
      reason: error.message,
    };
  }
}

/**
 * Check if two compile errors match
 */
function errorsMatch(a: CompileCompilerError, b: CompileResult): boolean {
  if (b.kind !== 'error') {
    return false;
  }
  return a.category === b.category && a.reason === b.reason;
}

/**
 * Convert AST to code string
 */
function astToCode(ast: t.File): string {
  return generate(ast).code;
}

/**
 * Clone an AST node deeply
 */
function cloneAst(ast: t.File): t.File {
  return t.cloneNode(ast, true);
}

/**
 * Generator that yields ASTs with statements removed one at a time
 */
function* removeStatements(ast: t.File): Generator<t.File> {
  // Collect all statement containers and their indices
  const statementContainers: Array<{
    statements: t.Statement[];
    index: number;
  }> = [];

  t.traverseFast(ast, node => {
    if (t.isBlockStatement(node) || t.isProgram(node)) {
      const body = node.body as t.Statement[];
      // Iterate in reverse order so removing later statements first
      for (let i = body.length - 1; i >= 0; i--) {
        statementContainers.push({statements: body, index: i});
      }
    }
  });

  for (const {statements, index} of statementContainers) {
    const cloned = cloneAst(ast);
    // Find the corresponding statements array in the cloned AST
    let found = false;
    t.traverseFast(cloned, node => {
      if (found) return;
      if (t.isBlockStatement(node) || t.isProgram(node)) {
        const body = node.body as t.Statement[];
        // Check if this is the same container by comparing original lengths
        // and ensuring the statement at the index exists
        if (body.length === statements.length && body[index]) {
          body.splice(index, 1);
          found = true;
        }
      }
    });
    if (found) {
      yield cloned;
    }
  }
}

/**
 * Generator that yields ASTs with call arguments removed one at a time
 */
function* removeCallArguments(ast: t.File): Generator<t.File> {
  // Collect all call expressions with their argument counts
  const callSites: Array<{callIndex: number; argCount: number}> = [];
  let callIndex = 0;
  t.traverseFast(ast, node => {
    if (t.isCallExpression(node) && node.arguments.length > 0) {
      callSites.push({callIndex, argCount: node.arguments.length});
      callIndex++;
    }
  });

  // For each call site, try removing each argument one at a time (from end to start)
  for (const {callIndex: targetCallIdx, argCount} of callSites) {
    for (let argIdx = argCount - 1; argIdx >= 0; argIdx--) {
      const cloned = cloneAst(ast);
      let idx = 0;
      let modified = false;

      t.traverseFast(cloned, node => {
        if (modified) return;
        if (t.isCallExpression(node) && node.arguments.length > 0) {
          if (idx === targetCallIdx && argIdx < node.arguments.length) {
            node.arguments.splice(argIdx, 1);
            modified = true;
          }
          idx++;
        }
      });

      if (modified) {
        yield cloned;
      }
    }
  }
}

/**
 * Generator that simplifies call expressions by replacing them with their arguments.
 * For single argument: foo(x) -> x
 * For multiple arguments: foo(x, y) -> [x, y]
 */
function* simplifyCallExpressions(ast: t.File): Generator<t.File> {
  // Count call expressions with arguments
  let callCount = 0;
  t.traverseFast(ast, node => {
    if (t.isCallExpression(node) && node.arguments.length > 0) {
      callCount++;
    }
  });

  // For each call, try replacing with arguments
  for (let targetIdx = 0; targetIdx < callCount; targetIdx++) {
    const cloned = cloneAst(ast);
    let idx = 0;
    let modified = false;

    traverse(cloned, {
      CallExpression(path) {
        if (modified) return;
        if (path.node.arguments.length > 0 && idx === targetIdx) {
          const args = path.node.arguments;
          // Filter to only Expression arguments (not SpreadElement)
          const exprArgs = args.filter(
            (arg): arg is t.Expression => t.isExpression(arg),
          );
          if (exprArgs.length === 0) {
            idx++;
            return;
          }
          if (exprArgs.length === 1) {
            // Single argument: replace call with the argument
            path.replaceWith(exprArgs[0]);
          } else {
            // Multiple arguments: replace call with array of arguments
            path.replaceWith(t.arrayExpression(exprArgs));
          }
          modified = true;
        }
        idx++;
      },
    });

    if (modified) {
      yield cloned;
    }
  }

  // Also try replacing with each individual argument for multi-arg calls
  for (let targetIdx = 0; targetIdx < callCount; targetIdx++) {
    // First, find the arg count for this call
    let argCount = 0;
    let currentIdx = 0;
    t.traverseFast(ast, node => {
      if (t.isCallExpression(node) && node.arguments.length > 0) {
        if (currentIdx === targetIdx) {
          argCount = node.arguments.length;
        }
        currentIdx++;
      }
    });

    // Try replacing with each argument individually
    for (let argIdx = 0; argIdx < argCount; argIdx++) {
      const cloned = cloneAst(ast);
      let idx = 0;
      let modified = false;

      traverse(cloned, {
        CallExpression(path) {
          if (modified) return;
          if (path.node.arguments.length > 0 && idx === targetIdx) {
            const arg = path.node.arguments[argIdx];
            if (t.isExpression(arg)) {
              path.replaceWith(arg);
              modified = true;
            }
          }
          idx++;
        },
      });

      if (modified) {
        yield cloned;
      }
    }
  }
}

/**
 * Generator that simplifies conditional expressions (a ? b : c) -> b or c
 */
function* simplifyConditionals(ast: t.File): Generator<t.File> {
  // Count conditionals
  let condCount = 0;
  t.traverseFast(ast, node => {
    if (t.isConditionalExpression(node)) {
      condCount++;
    }
  });

  // For each conditional, try replacing with consequent
  for (let targetIdx = 0; targetIdx < condCount; targetIdx++) {
    const cloned = cloneAst(ast);
    let modified = false;
    let idx = 0;

    traverse(cloned, {
      ConditionalExpression(path) {
        if (modified) return;
        if (idx === targetIdx) {
          path.replaceWith(path.node.consequent);
          modified = true;
        }
        idx++;
      },
    });

    if (modified) {
      yield cloned;
    }
  }

  // Also try replacing with alternate
  for (let targetIdx = 0; targetIdx < condCount; targetIdx++) {
    const cloned = cloneAst(ast);
    let modified = false;
    let idx = 0;

    traverse(cloned, {
      ConditionalExpression(path) {
        if (modified) return;
        if (idx === targetIdx) {
          path.replaceWith(path.node.alternate);
          modified = true;
        }
        idx++;
      },
    });

    if (modified) {
      yield cloned;
    }
  }
}

/**
 * Generator that simplifies logical expressions (a && b) -> a or b
 */
function* simplifyLogicalExpressions(ast: t.File): Generator<t.File> {
  // Count logical expressions
  let logicalCount = 0;
  t.traverseFast(ast, node => {
    if (t.isLogicalExpression(node)) {
      logicalCount++;
    }
  });

  // Try replacing with left side
  for (let targetIdx = 0; targetIdx < logicalCount; targetIdx++) {
    const cloned = cloneAst(ast);
    let idx = 0;
    let modified = false;

    traverse(cloned, {
      LogicalExpression(path) {
        if (modified) return;
        if (idx === targetIdx) {
          path.replaceWith(path.node.left);
          modified = true;
        }
        idx++;
      },
    });

    if (modified) {
      yield cloned;
    }
  }

  // Try replacing with right side
  for (let targetIdx = 0; targetIdx < logicalCount; targetIdx++) {
    const cloned = cloneAst(ast);
    let idx = 0;
    let modified = false;

    traverse(cloned, {
      LogicalExpression(path) {
        if (modified) return;
        if (idx === targetIdx) {
          path.replaceWith(path.node.right);
          modified = true;
        }
        idx++;
      },
    });

    if (modified) {
      yield cloned;
    }
  }
}

/**
 * Generator that simplifies optional chains (a?.b) -> a.b
 */
function* simplifyOptionalChains(ast: t.File): Generator<t.File> {
  // Count optional expressions
  let optionalCount = 0;
  t.traverseFast(ast, node => {
    if (t.isOptionalMemberExpression(node) || t.isOptionalCallExpression(node)) {
      optionalCount++;
    }
  });

  for (let targetIdx = 0; targetIdx < optionalCount; targetIdx++) {
    const cloned = cloneAst(ast);
    let idx = 0;
    let modified = false;

    traverse(cloned, {
      OptionalMemberExpression(path) {
        if (modified) return;
        if (idx === targetIdx) {
          const {object, property, computed} = path.node;
          path.replaceWith(t.memberExpression(object, property, computed));
          modified = true;
        }
        idx++;
      },
      OptionalCallExpression(path) {
        if (modified) return;
        if (idx === targetIdx) {
          const {callee, arguments: args} = path.node;
          if (t.isExpression(callee)) {
            path.replaceWith(t.callExpression(callee, args));
            modified = true;
          }
        }
        idx++;
      },
    });

    if (modified) {
      yield cloned;
    }
  }
}

/**
 * Generator that removes array elements one at a time
 */
function* removeArrayElements(ast: t.File): Generator<t.File> {
  // Collect all array expressions with their element counts
  const arraySites: Array<{arrayIndex: number; elementCount: number}> = [];
  let arrayIndex = 0;
  t.traverseFast(ast, node => {
    if (t.isArrayExpression(node) && node.elements.length > 0) {
      arraySites.push({arrayIndex, elementCount: node.elements.length});
      arrayIndex++;
    }
  });

  // For each array, try removing each element one at a time (from end to start)
  for (const {arrayIndex: targetArrayIdx, elementCount} of arraySites) {
    for (let elemIdx = elementCount - 1; elemIdx >= 0; elemIdx--) {
      const cloned = cloneAst(ast);
      let idx = 0;
      let modified = false;

      t.traverseFast(cloned, node => {
        if (modified) return;
        if (t.isArrayExpression(node) && node.elements.length > 0) {
          if (idx === targetArrayIdx && elemIdx < node.elements.length) {
            node.elements.splice(elemIdx, 1);
            modified = true;
          }
          idx++;
        }
      });

      if (modified) {
        yield cloned;
      }
    }
  }
}

/**
 * Generator that replaces single-element arrays with the element itself
 */
function* simplifySingleElementArrays(ast: t.File): Generator<t.File> {
  // Count single-element arrays
  let arrayCount = 0;
  t.traverseFast(ast, node => {
    if (t.isArrayExpression(node) && node.elements.length === 1) {
      arrayCount++;
    }
  });

  for (let targetIdx = 0; targetIdx < arrayCount; targetIdx++) {
    const cloned = cloneAst(ast);
    let idx = 0;
    let modified = false;

    traverse(cloned, {
      ArrayExpression(path) {
        if (modified) return;
        if (path.node.elements.length === 1 && idx === targetIdx) {
          const elem = path.node.elements[0];
          if (t.isExpression(elem)) {
            path.replaceWith(elem);
            modified = true;
          }
        }
        idx++;
      },
    });

    if (modified) {
      yield cloned;
    }
  }
}

/**
 * Generator that removes object properties one at a time
 */
function* removeObjectProperties(ast: t.File): Generator<t.File> {
  // Collect all object expressions with their property counts
  const objectSites: Array<{objectIndex: number; propCount: number}> = [];
  let objectIndex = 0;
  t.traverseFast(ast, node => {
    if (t.isObjectExpression(node) && node.properties.length > 0) {
      objectSites.push({objectIndex, propCount: node.properties.length});
      objectIndex++;
    }
  });

  // For each object, try removing each property one at a time (from end to start)
  for (const {objectIndex: targetObjIdx, propCount} of objectSites) {
    for (let propIdx = propCount - 1; propIdx >= 0; propIdx--) {
      const cloned = cloneAst(ast);
      let idx = 0;
      let modified = false;

      t.traverseFast(cloned, node => {
        if (modified) return;
        if (t.isObjectExpression(node) && node.properties.length > 0) {
          if (idx === targetObjIdx && propIdx < node.properties.length) {
            node.properties.splice(propIdx, 1);
            modified = true;
          }
          idx++;
        }
      });

      if (modified) {
        yield cloned;
      }
    }
  }
}

/**
 * Generator that simplifies binary expressions (a + b) -> a or b
 */
function* simplifyBinaryExpressions(ast: t.File): Generator<t.File> {
  // Count binary expressions
  let binaryCount = 0;
  t.traverseFast(ast, node => {
    if (t.isBinaryExpression(node)) {
      binaryCount++;
    }
  });

  // Try replacing with left side
  for (let targetIdx = 0; targetIdx < binaryCount; targetIdx++) {
    const cloned = cloneAst(ast);
    let idx = 0;
    let modified = false;

    traverse(cloned, {
      BinaryExpression(path) {
        if (modified) return;
        if (idx === targetIdx) {
          path.replaceWith(path.node.left);
          modified = true;
        }
        idx++;
      },
    });

    if (modified) {
      yield cloned;
    }
  }

  // Try replacing with right side
  for (let targetIdx = 0; targetIdx < binaryCount; targetIdx++) {
    const cloned = cloneAst(ast);
    let idx = 0;
    let modified = false;

    traverse(cloned, {
      BinaryExpression(path) {
        if (modified) return;
        if (idx === targetIdx) {
          path.replaceWith(path.node.right);
          modified = true;
        }
        idx++;
      },
    });

    if (modified) {
      yield cloned;
    }
  }
}

/**
 * Generator that simplifies member expressions (obj.value) -> obj
 */
function* simplifyMemberExpressions(ast: t.File): Generator<t.File> {
  // Count member expressions
  let memberCount = 0;
  t.traverseFast(ast, node => {
    if (t.isMemberExpression(node)) {
      memberCount++;
    }
  });

  for (let targetIdx = 0; targetIdx < memberCount; targetIdx++) {
    const cloned = cloneAst(ast);
    let idx = 0;
    let modified = false;

    traverse(cloned, {
      MemberExpression(path) {
        if (modified) return;
        if (idx === targetIdx) {
          path.replaceWith(path.node.object);
          modified = true;
        }
        idx++;
      },
    });

    if (modified) {
      yield cloned;
    }
  }
}

/**
 * All simplification strategies in order of priority (coarse to fine)
 */
const simplificationStrategies = [
  {name: 'removeStatements', generator: removeStatements},
  {name: 'removeCallArguments', generator: removeCallArguments},
  {name: 'removeArrayElements', generator: removeArrayElements},
  {name: 'removeObjectProperties', generator: removeObjectProperties},
  {name: 'simplifyCallExpressions', generator: simplifyCallExpressions},
  {name: 'simplifyConditionals', generator: simplifyConditionals},
  {name: 'simplifyLogicalExpressions', generator: simplifyLogicalExpressions},
  {name: 'simplifyBinaryExpressions', generator: simplifyBinaryExpressions},
  {name: 'simplifySingleElementArrays', generator: simplifySingleElementArrays},
  {name: 'simplifyMemberExpressions', generator: simplifyMemberExpressions},
  {name: 'simplifyOptionalChains', generator: simplifyOptionalChains},
];

type MinimizeResult =
  | {kind: 'success'}
  | {kind: 'minimal'}
  | {kind: 'minimized'; source: string};

/**
 * Core minimization loop that attempts to reduce the input source code
 * while preserving the compiler error.
 */
export function minimize(
  input: string,
  filename: string,
  language: 'flow' | 'typescript',
  sourceType: 'module' | 'script',
): MinimizeResult {
  // Load the compiler plugin
  const importedCompilerPlugin = require(PROJECT_SRC) as Record<
    string,
    unknown
  >;
  const BabelPluginReactCompiler = importedCompilerPlugin[
    'default'
  ] as PluginObj;
  const parseConfigPragmaForTests = importedCompilerPlugin[
    PARSE_CONFIG_PRAGMA_IMPORT
  ] as typeof ParseConfigPragma;

  // Get the initial error
  const initialResult = compileAndGetError(
    input,
    filename,
    language,
    sourceType,
    BabelPluginReactCompiler,
    parseConfigPragmaForTests,
  );

  if (initialResult.kind === 'success') {
    return {kind: 'success'};
  }

  if (initialResult.kind === 'parse_error') {
    return {kind: 'success'};
  }

  const targetError = initialResult;

  // Parse the initial AST
  let currentAst = parseInput(input, filename, language, sourceType);
  let currentCode = input;
  let changed = true;
  let iterations = 0;
  const maxIterations = 1000; // Safety limit

  process.stdout.write('\nMinimizing');

  while (changed && iterations < maxIterations) {
    changed = false;
    iterations++;

    // Try each simplification strategy
    for (const strategy of simplificationStrategies) {
      const generator = strategy.generator(currentAst);

      for (const candidateAst of generator) {
        let candidateCode: string;
        try {
          candidateCode = astToCode(candidateAst);
        } catch {
          // If code generation fails, skip this candidate
          continue;
        }

        const result = compileAndGetError(
          candidateCode,
          filename,
          language,
          sourceType,
          BabelPluginReactCompiler,
          parseConfigPragmaForTests,
        );

        if (errorsMatch(targetError, result)) {
          // This simplification preserves the error, keep it
          currentAst = candidateAst;
          currentCode = candidateCode;
          changed = true;
          process.stdout.write('.');
          break; // Restart from the beginning with the new AST
        }
      }

      if (changed) {
        break; // Restart the outer loop
      }
    }
  }

  console.log('\n');

  // Check if any minimization was achieved
  if (currentCode === input) {
    return {kind: 'minimal'};
  }

  return {kind: 'minimized', source: currentCode};
}

/**
 * Main minimize function that reads the input file, runs minimization,
 * and reports results.
 */
export async function runMinimize(options: MinimizeOptions): Promise<void> {
  // Resolve the input path
  const inputPath = path.isAbsolute(options.path)
    ? options.path
    : path.resolve(process.cwd(), options.path);

  // Check if file exists
  if (!fs.existsSync(inputPath)) {
    console.error(`Error: File not found: ${inputPath}`);
    process.exit(1);
  }

  // Read the input file
  const input = fs.readFileSync(inputPath, 'utf-8');
  const filename = path.basename(inputPath);
  const firstLine = input.substring(0, input.indexOf('\n'));
  const language = parseLanguage(firstLine);
  const sourceType = parseSourceType(firstLine);

  console.log(`Minimizing: ${inputPath}`);

  const originalLines = input.split('\n').length;

  // Run the minimization
  const result = minimize(input, filename, language, sourceType);

  if (result.kind === 'success') {
    console.log('Could not minimize: the input compiles successfully.');
    process.exit(0);
  }

  if (result.kind === 'minimal') {
    console.log(
      'Could not minimize: the input fails but is already minimal and cannot be reduced further.',
    );
    process.exit(0);
  }

  // Output the minimized code
  console.log('--- Minimized Code ---');
  console.log(result.source);

  const minimizedLines = result.source.split('\n').length;
  console.log(
    `\nReduced from ${originalLines} lines to ${minimizedLines} lines`,
  );
}
