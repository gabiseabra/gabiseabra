import {debounce} from 'debounce'
import {gsap} from 'gsap'

const noop = () => null

let $id = 0
const mkId = () => $id++

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

  return Math.max(0, y)
}

const UP = 1,
  DOWN = -1

let viewHeight, scrollMax
let lastY = window.scrollY
let delta, direction
let touching = false
let snapPoints = []
let scrollTriggers = []

export const getProgress = () => window.scrollY / scrollMax

/**
 * Recalculate layout
 */
const update = debounce(function update() {
  viewHeight = window.innerHeight
  scrollMax = document.body.scrollHeight - window.innerHeight
  snapPoints = snapPoints.map((x) => ({...x, ...calc(x.element)}))
  scrollTriggers = scrollTriggers.map((x) => ({...x, ...calc(x.element)}))

  console.log({viewHeight, scrollMax, scrollTriggers})
}, 100)

/**
 * Mutation observer watches for changes in immediate children of each
 * trigger to recalculate the layout when needed.
 */
const observer = new MutationObserver(update)
const observerOpts = {subtree: false, childList: true}

function onScroll() {
  const y = window.scrollY
  delta = y - lastY
  direction = y > lastY ? DOWN : UP
  updateScrollTrigger()
  updateSnapPoint()
}

const intersects = ({start, end}, y) => start <= y && end > y

function updateScrollTrigger() {
  const current = scrollTriggers.current
  const threshold = window.scrollY + viewHeight / 2

  if (current && intersects(current, threshold)) return

  for (let i = 0; i < scrollTriggers.length; ++i) {
    const trigger = scrollTriggers[i]
    if (intersects(trigger, threshold)) {
      scrollTriggers.current = trigger
      requestIdleCallback(trigger.onEnter)
      break
    }
  }
}

const updateSnapPoint = debounce(function updateSnapPoint() {
  const y = getProgress()
  let minIdx,
    minDiff = 1

  for (let i = 0; i < snapPoints.length; ++i) {
    const d = Math.abs(y - snapPoints[i].position)
    if (minDiff > d) {
      minIdx = i
      minDiff = d
    }
  }

  if (minDiff === 0) return

  console.log(snapPoints, y, minIdx)
  // console.log(y, snapPoints[minIdx])
  // gsap.to(window, {
  //   scrollY: snapPoints[minIdx] * scrollMax
  // })
}, 100)

function calc(element) {
  const height = element.offsetHeight
  const start = offsetTop(element)
  const end = start + height
  const position = start / scrollMax
  return {
    element,
    position,
    height,
    start,
    end
  }
}

export function setSnapPoints(elements) {
  snapPoints = elements.map(calc)
}

export function mkScrollTrigger(element, onEnter) {
  const id = mkId()
  scrollTriggers.push({id, onEnter, ...calc(element)})
  observer.observe(element, observerOpts)
  return () => {
    const idx = scrollTriggers.findIndex((trigger) => trigger.id === id)
    scrollTriggers.splice(idx, 1)
    observer.disconnect()
    scrollTriggers.forEach(({element}) =>
      observer.observe(element, observerOpts)
    )
  }
}

export function init() {
  window.addEventListener('scroll', onScroll)
  window.addEventListener('resize', update, {passive: true})
  window.addEventListener(
    'touchstart',
    () => {
      touching = true
    },
    {passive: true}
  )
  window.addEventListener(
    'touchend',
    () => {
      touching = false
    },
    {passive: true}
  )
  update()
}
