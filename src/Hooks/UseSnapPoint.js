"use strict"

const { gsap } = require('gsap')
const { ScrollTrigger } = require('gsap/ScrollTrigger')

const TRIGGER_ID = 'SNAP_POINTS'

const CALLBACK = Symbol('SNAP_POINT')

const percent = (min, max) => (x) => gsap.utils.mapRange(min, max, 0, 1, x)

const createTrigger = (opts) => {
  const trigger = ScrollTrigger.getById(opts.id)
  if (trigger) trigger.kill()
  return ScrollTrigger.create(opts)
}

window[CALLBACK] = () => null

// Re-calculate snap trigger every time ScrollTrigger refreshes (resize events)
ScrollTrigger.addEventListener('refresh', () => {
  if (window[CALLBACK].timeout) clearTimeout(window[CALLBACK].timeout)
  window[CALLBACK].timeout = setTimeout(window[CALLBACK], 210)
})

exports.setSnapPoints = (points) => () => {
  window[CALLBACK] = () => {
    const max = document.body.scrollHeight - window.innerHeight
    createTrigger({
      markers: true,
      id: TRIGGER_ID,
      start: 0,
      end: max,
      snap: {
        snapTo: points.reduce((p, fun) => p.concat(fun().map(percent(0, max))), []),
        duration: { min: 0.1, max: 0.3 },
        ease: "circ.inOut"
      }
    })
  }
  // Update snap points immediately
  window[CALLBACK]()
}