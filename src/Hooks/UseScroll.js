'use strict'

const {gsap} = require('gsap')
const {ScrollTrigger} = require('gsap/ScrollTrigger')

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
  } while (el)

  return y
}

window.Scroller = {
  idx: 0,
  children: [],
  listeners: new Set(),
  timeout: undefined,
  trigger: undefined,
  get height() {
    return document.body.offsetHeight - window.innerHeight
  },
  addEventListener(fn) {
    return this.listeners.add(fn)
  },
  removeEventListener(fn) {
    return this.listeners.delete(fn)
  }
}

exports.mkIdx = () => Scroller.idx++

exports.setSnapPoints = (children) => () => {
  if (Scroller.trigger) Scroller.trigger.kill()

  Scroller.children = children
  Scroller.snapPoints = Array.from(children).map(offsetTop)

  const start = 0
  const end = Scroller.height
  const snap = {
    snapTo: Scroller.snapPoints.map(percent(start, end)),
    duration: {min: 0.3, max: 0.5},
    ease: 'power1.inOut'
  }

  Scroller.trigger = ScrollTrigger.create({
    start,
    end,
    snap,
    onUpdate(...args) {
      for (let fn of Scroller.listeners) fn(...args)
    }
  })

  ScrollTrigger.refresh()
}

exports.mkScrollTrigger = ({onEnter, onEnterBack}) => (trigger) => () => {
  const effect = (fn) => (a) => fn(a)()
  return ScrollTrigger.create({
    trigger,
    start: 'top 50%',
    end: 'bottom 50%',
    onEnter: effect(onEnter),
    onEnterBack: effect(onEnterBack)
  })
}

exports.snapTo = (el) => () => {
  const top = offsetTop(el)
  const distances = Scroller.snapPoints.map((p) => Math.abs(p - top))
  const idx = distances.indexOf(Math.min(...distances))
  window.scrollTo({top: Scroller.snapPoints[idx], behavior: 'smooth'})
}

exports.kill = (trigger) => () => trigger.kill()
