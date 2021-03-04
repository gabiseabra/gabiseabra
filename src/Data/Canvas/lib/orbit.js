import * as THREE from 'three'

/**
 * Orbit bits extracted from `OrbitControls` and adapted to mousemove event
 * @see https://github.com/mrdoob/three.js/blob/master/examples/jsm/controls/OrbitControls.js#L152
 */

export const mkOrbitControl = (canvas, options = {}) => {
  const orbit = mkOrbit(canvas, options)
  const pos = new THREE.Vector2()
  const listener = (e) => {
    const x = e.clientX / window.innerWidth
    const y = e.clientY / window.innerHeight
    pos.set(x, y).sub(orbit.options.axis).multiplyScalar(2)
    tilt(pos, orbit)
  }
  document.addEventListener('mousemove', listener)
  canvas.listeners.push(() =>
    document.removeEventListener('mousemove', listener)
  )
}

const defOptions = {
  center: new THREE.Vector3(0, 0, 0),
  axis: new THREE.Vector3(0.5, 0.5),
  polarAngle: Math.PI / 2,
  azimuthAngle: Math.PI / 2
}

const mkOrbit = ({camera}, options = {}) => {
  const quat = new THREE.Quaternion().setFromUnitVectors(
    camera.up,
    new THREE.Vector3(0, 1, 0)
  )
  return {
    camera,
    quat,
    quatInverse: quat.clone().invert(),
    spherical: new THREE.Spherical(),
    offset: new THREE.Vector3(),
    position: camera.position.clone(),
    options: {...defOptions, ...options}
  }
}

function tilt(
  pos,
  {options, camera, quat, quatInverse, offset, spherical, position}
) {
  offset.copy(position).sub(options.center)

  // rotate offset to "y-axis-is-up" space
  offset.applyQuaternion(quat)
  // angle from z-axis around y-axis
  spherical.setFromVector3(offset)

  spherical.theta -= pos.x * options.azimuthAngle
  spherical.phi -= pos.y * options.polarAngle

  spherical.makeSafe()

  offset.setFromSpherical(spherical)

  // rotate offset back to "camera-up-vector-is-up" space
  offset.applyQuaternion(quatInverse)

  camera.position.copy(options.center).add(offset)

  camera.lookAt(options.center)
}
