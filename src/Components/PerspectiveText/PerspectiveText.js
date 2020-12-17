"use strict"

const { gsap } = require('gsap')
const { ScrollTrigger } = require('gsap/ScrollTrigger')

const styles = require('./PerspectiveText.scss')

const translateX = (e) => (idx) => (e.clientX - (window.innerWidth / 2)) * (idx + 1) * 0.005
const translateY = (e) => (idx) => (e.clientY - (window.innerHeight / 2)) * (idx + 1) * 0.005

exports.animate = (trigger) => {
  const layers = trigger.querySelectorAll(`.${styles.layers} span`)
  const before = trigger.querySelector(`.${styles.before} span`)
  const after = trigger.querySelector(`.${styles.after} span`)
  const animateLayers = (e) => {
    gsap.to(layers, {
      duration: 0.5,
      x: translateX(e),
      y: translateY(e)
    })
    gsap.to(before, {
      x: translateX(e)(layers.length),
      y: translateY(e)(layers.length)
    })
    gsap.to(after, {
      x: translateX(e)(layers.length),
      y: translateY(e)(layers.length)
    })
  }
  const onEnter = () => document.addEventListener('mousemove', animateLayers)
  const onLeave = () => document.removeEventListener('mousemove', animateLayers)
  ScrollTrigger.create({
    trigger: trigger,
    onLeave,
    onEnter,
    onEnterBack: onEnter,
  })
}

exports.styles = styles
