"use strict"

exports.styles = require('./RepoList.scss')

const body = document.body;
const docEl = document.documentElement;

exports.offsetTop = (el) => () => {
  const box = el.getBoundingClientRect()

  const scrollTop = window.pageYOffset || docEl.scrollTop || body.scrollTop

  const clientTop = docEl.clientTop || body.clientTop || 0

  return box.top + scrollTop - clientTop
}
