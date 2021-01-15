"use strict"

const { gsap } = require('gsap')
const { ScrollTrigger } = require('gsap/ScrollTrigger')

const TRIGGER = Symbol('TRIGGER')

const percent = (min, max) => (x) => gsap.utils.mapRange(min, max, 0, 1, x)

/**
 * Get an element's offset top relative to the document root without transforms.
 * @param {HTMLElement} target
 */
const offsetTop = (target) => {
  let el = target
  let y = 0

  do {
    y += el.offsetTop
    el = el.offsetParent
  } while(el)

  return y
}

window.Scroller = {
  idx: 0,
  children: [],
  listeners: new Set(),
  timeout: undefined,
  get trigger() { return this[TRIGGER] },
  set trigger(t) {
    if (this[TRIGGER]) this[TRIGGER].kill()
    this[TRIGGER] = t
  },
  get height() { return document.getElementById('root').offsetHeight - window.innerHeight },
  addEventListener(fn) {
    return this.listeners.add(fn)
  },
  removeEventListener(fn) {
    return this.listeners.delete(fn)
  },
  getSnapPoints() {
    return Array.from(this.children).map(offsetTop)
  }
}

exports.setSnapPoints = (children) => () => {
  Scroller.children = children

  const start = 0
  const end = Scroller.height
  const snap = {
    snapTo: Scroller.getSnapPoints().map(percent(start, end)),
    duration: { min: 0.1, max: 0.3 },
    ease: "circ.inOut"
  }

  Scroller.trigger = ScrollTrigger.create({
    start,
    end,
    snap,
    onUpdate(...args) {
      for(let fn of Scroller.listeners) fn(...args)
    }
  })
}

exports.mkIdx = () => Scroller.idx++
