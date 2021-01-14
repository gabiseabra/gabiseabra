"use strict"

const { gsap } = require('gsap')
const { ScrollTrigger } = require('gsap/ScrollTrigger')

const TRIGGER = Symbol('TRIGGER')

window.Scroller = {
  trigger: undefined,
  element: undefined,
  listeners: new Set(),
  timeout: undefined,
  maxAngle: Math.PI / 2,
  get trigger() { return this[TRIGGER] },
  set trigger(t) {
    if (this[TRIGGER]) this[TRIGGER].kill()
    this[TRIGGER] = t
  },
  get height() { return document.getElementById('root').offsetHeight },
  addEventListener(fn) {
    return this.listeners.add(fn)
  },
  removeEventListener(fn) {
    return this.listeners.delete(fn)
  },
  update() {}
}

Scroller.addEventListener(({ progress, end }) => {
  const el = Scroller.element
  gsap.set(el, {
    y: progress * end,
    rotateX: `-${progress * Scroller.maxAngle}rad`
  })
})

// Re-calculate snap trigger every time ScrollTrigger refreshes (resize events)
// ScrollTrigger.addEventListener('refresh', () => {
//   if (Scroller.timeout) clearTimeout(Scroller.timeout)
//   Scroller.timeout = setTimeout(Scroller.update, 210)
// })

const percent = (min, max) => (x) => gsap.utils.mapRange(min, max, 0, 1, x)

/**
 * Get an element's offset top relative to the document root without transforms.
 * @param {HTMLElement} target
 */
exports.offsetTop = (target) => () => {
  let el = target
  let y = 0

  do {
    y += el.offsetTop
    el = el.offsetParent
  } while(el)

  return y
}

const updateChildNode = (r) => ({offset, rotation}, el) => {
  const he = el.offsetHeight
  // element's arc length
  const arc = el.offsetHeight
  // element's angle
  const angle = arc / r

  gsap.set(el, {
    z: -r,
    rotateX: rotation + 'rad',
    transformOrigin: `50% 50% ${r}px`,
  })

  return {
    offset: offset + he,
    rotation: rotation + angle
  }
}

exports.updateScroller = (el) => () => {
  Scroller.element = el
  const hs = Scroller.height
  const r = hs / Scroller.maxAngle
  gsap.set(el, { z: r })
  Array.from(el.childNodes).reduce(updateChildNode(r), {offset: 0, rotation: 0})
}

exports.setSnapPoints = (points) => () => {
  const start = 0
  const end = Scroller.height - window.innerHeight
  const snap = {
    snapTo: points.reduce((p, fun) =>
      p.concat(fun().map(percent(start, end))),
      [0]
    ),
    duration: { min: 0.1, max: 0.3 },
    ease: "circ.inOut"
  }

  /*
  Scroller.trigger = gsap.to(Scroller.element, {
    rotateX: -Scroller.maxAngle + 'rad',
    scrollTrigger: {
      trigger: document.getElementById('root'),
      pin: true,
      pinSpacing: false,
      scrub: true,
      start,
      end,
      snap,
      onUpdate(...args) {
        for(let fn of Scroller.listeners) fn(...args)
      }
    }
  })
  */

  Scroller.trigger = ScrollTrigger.create({
    markers: true,
    scrub: true,
    start,
    end,
    snap,
    onUpdate(...args) {
      for(let fn of Scroller.listeners) fn(...args)
    }
  })
}
