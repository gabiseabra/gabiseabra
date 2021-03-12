'use strict'

const scroller = require('../lib/scroller')

window.Scroller = {}

let $id = 0

exports.mkId = () => $id++

exports.setSnapPoints = (elements) => () => scroller.setSnapPoints(elements)

exports.mkScrollTrigger = ({onEnter}) => (element) => () => {
  const kill = scroller.mkScrollTrigger(element, onEnter)
  return {kill}
}

exports.snapTo = (el) => () => {
  return
  const top = offsetTop(el)
  const distances = Scroller.snapPoints.map((p) => Math.abs(p - top))
  const idx = distances.indexOf(Math.min(...distances))
  window.scrollTo({top: Scroller.snapPoints[idx], behavior: 'smooth'})
}

exports.kill = (trigger) => () => trigger.kill()
