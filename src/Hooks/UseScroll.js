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

exports.getScrollHeight = () => document.body.scrollHeight - window.innerHeight

exports.setSnapPoints = (points) => () => {
  const start = 0
  const end = exports.getScrollHeight()
  window[CALLBACK] = () => {
    createTrigger({
      markers: true,
      id: TRIGGER_ID,
      start,
      end,
      onUpdate(...args) {
        if(window.onScroll) window.onScroll(...args)
      },
      snap: {
        snapTo: points.reduce((p, fun) => p.concat(fun().map(percent(start, end))), []),
        duration: { min: 0.1, max: 0.3 },
        ease: "circ.inOut"
      }
    })
  }
  window[CALLBACK]()
}
