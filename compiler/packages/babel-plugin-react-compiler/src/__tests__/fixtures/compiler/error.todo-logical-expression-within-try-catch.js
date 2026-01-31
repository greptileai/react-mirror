function Component(props) {
  const x = [];
  let result;
  try {
    result = bar(props.cond && props.foo);
  } catch (e) {
    console.log(e);
  }
  x.push(result);
  return <Stringify x={x} />;
}
