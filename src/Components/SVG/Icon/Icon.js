"use strict"

const def = ({icon}) => ({ width: icon[0], height: icon[1], path: icon[4] })

const mapDefs = (obj) => Object.keys(obj).reduce((defs, key) => Object.assign(defs, { [key]: def(obj[key]) }), {})

exports.icons = mapDefs({
  clock: require('@fortawesome/free-solid-svg-icons/faClock').definition,
  github: require('@fortawesome/free-brands-svg-icons/faGithubAlt').definition,
  externalLink: require('@fortawesome/free-solid-svg-icons/faExternalLinkAlt').definition,
})
