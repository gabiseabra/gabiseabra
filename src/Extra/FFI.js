exports.instanceOf = (name) => (a) => {
  if (typeof a === 'undefined' || a === null) return false
  return Object.getPrototypeOf(a).constructor.name === name
}