"use strict"

const {gsap} = require('gsap')

const styles = require('./RepoList.css')

const rad = 135

const invert = (fun) => (idx, el, items) => fun((items.length - 1) - idx, el, items)

const opacity = (idx, el, items) => {
  const length = items.length - 1
  return gsap.utils.mapRange(
    0, length,
    1, 0.2,
    idx
  )
}

const rotateX = (idx, el, items) => {
  const length = items.length - 1
  return -1 * rad * (idx / length)
}

const translateZ = (height, length) => {
  return Math.round((height / 2) / Math.tan(Math.PI / length));
}

exports.animate = (container) => {
  const scene = container.querySelector("." + styles.scene)
  const list = container.querySelector("." + styles.list)
  const items = container.querySelectorAll("." + styles.item)
  const length = items.length - 1

  const start = () =>  container.offsetTop - window.innerHeight * 0.25
  const end = () => '+=' + container.offsetHeight

  const z = translateZ(window.innerHeight * 0.5, length * (360 / rad))

  // Transform each carousel item in 3d space
  gsap.set(items, {
    transformOrigin: '50% 50% ' + (-z),
    rotateX,
    opacity,
    z
  })
  gsap.set(list, { z: -1 * z })
  // Rotate carousel on scroll
  gsap.to(list, {
    rotateX: rad,
    scrollTrigger: {
      target: container,
      pin: scene,
      anticipatePin: 1,
      scrub: true,
      start,
      end
    }
  })
  // Animate opacity of carousel items
  const timeline = gsap.timeline({
    scrollTrigger: {
      target: container,
      scrub: true,
      start,
      end
    }
  })
  items.forEach((it, idx) => {
    timeline
      .to(it, { opacity: 1 }, idx / items)
      .to(it, { opacity: invert(opacity)(idx, it, items) }, 1)
  })
}

exports.styles = styles
