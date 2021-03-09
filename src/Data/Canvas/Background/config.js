const THREE = require('three')
const config = require('three-playground/src/theme/config').default

config.cloud.shadowColor = 0xba7f93
config.sun.material = new THREE.MeshBasicMaterial({
  flatShading: true,
  color: 0xff667a
})

module.exports = config
