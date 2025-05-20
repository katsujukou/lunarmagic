export const current = (() => {
  Error.stackTraceLimit = 20;
  try {
    throw new Error();
  }
  catch (e) {
    if (!e.stack) {
      return [];
    }
    return e.stack.split("\n").slice(0);
  }
})();