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
