'use strict'

exports.styles = require('./Menu.scss')

exports.attachMouseMove = (f) => () => {
  const listener = (e) => {
    const maxX = window.innerWidth
    const maxY = window.innerHeight
    const x = e.x / maxX
    const y = e.y / maxY
    f({x, y})()
  }

  window.addEventListener('mousemove', listener, true)

  return () => window.removeEventListener('mousemove', listener, true)
}
