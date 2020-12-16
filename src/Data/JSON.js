const isValidDate = (d) => d instanceof Date && !isNaN(d)

exports.parseDateTime = (data) => {
  const date = new Date(data)
  if (!isValidDate(date)) return null
  return {
    year: date.getFullYear(),
    month: date.getMonth() + 1,
    day: date.getDay() + 1,
    hour: date.getHours(),
    minute: date.getMinutes(),
    second: date.getSeconds(),
    millisecond: date.getMilliseconds()
  }
}
