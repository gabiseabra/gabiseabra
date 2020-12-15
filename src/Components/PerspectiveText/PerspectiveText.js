"use strict"

const { gsap } = require('gsap')
const { ScrollTrigger } = require('gsap/ScrollTrigger')

const styles = require('./PerspectiveText.css')

const translateX = (e) => (idx) => (e.clientX - (window.innerHeight / 2)) * (idx + 1) * 0.01
const translateY = (e) => (idx) => (e.clientY - (window.innerWidth / 2)) * (idx + 1) * 0.01

exports.animate = (trigger) => {
  console.log(trigger)
  const layers = trigger.querySelectorAll(`.${styles.layers} span`)
  const animateLayers = (e) => {
    gsap.to(layers, {
      duration: 0.5,
      x: translateX(e),
      y: translateY(e)
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
