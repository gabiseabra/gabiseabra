"use strict"

const { gsap } = require('gsap')
const { ScrollTrigger } = require('gsap/ScrollTrigger')

const TRIGGER_ID = 'snapPoints'

const percent = (min, max) => (x) => gsap.utils.mapRange(min, max, 0, 1, x)

exports.setSnapPoints = (points) => () => {
  const max = document.body.scrollHeight
  const trigger = ScrollTrigger.getById(TRIGGER_ID)
  if (trigger) trigger.kill()
  if (points.length === 0) return
  ScrollTrigger.create({
    id: TRIGGER_ID,
    start: 0,
    end: max,
    snap: {
      snapTo: points.reduce((p, fun) => p.concat(fun().map(percent(0, max))), []),
      duration: { min: 0.05, max: 0.2 },
      ease: "circ.inOut"
    }
  })
}