
## Input

```javascript
function Component(props) {
  const x = [];
  let result;
  for (let i = 0; i < 10; i++) {
    if (cond) {
      try {
        result = {key: bar([props.cond && props.foo])};
      } catch (e) {
        console.log(e);
      }
    }
  }
  x.push(result);
  return <Stringify x={x} />;
}

```


## Error

```
Found 1 error:

Todo: Support value blocks (conditional, logical, optional chaining, etc) within a try/catch statement

error.todo-logical-expression-within-try-catch.ts:7:28
   5 |     if (cond) {
   6 |       try {
>  7 |         result = {key: bar([props.cond && props.foo])};
     |                             ^^^^^ Support value blocks (conditional, logical, optional chaining, etc) within a try/catch statement
   8 |       } catch (e) {
   9 |         console.log(e);
  10 |       }
```
          
      