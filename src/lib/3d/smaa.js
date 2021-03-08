import * as THREE from 'three'
const {
  SMAAImageLoader,
  SMAAEffect,
  SMAAPreset,
  EdgeDetectionMode
} = require('postprocessing')

function load() {
  const assets = new Map()
  const loadingManager = new THREE.LoadingManager()
  const smaaImageLoader = new SMAAImageLoader(loadingManager)

  return new Promise((resolve, reject) => {
    loadingManager.onLoad = () => resolve(assets)
    loadingManager.onError = reject

    smaaImageLoader.load(([search, area]) => {
      assets.set('smaa-search', search)
      assets.set('smaa-area', area)
    })
  })
}

export function init() {
  load().then((assets) => {
    new SMAAEffect(
      assets.get('smaa-search'),
      assets.get('smaa-area'),
      SMAAPreset.HIGH,
      EdgeDetectionMode.DEPTH
    )
  })
}
