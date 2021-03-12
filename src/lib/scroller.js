import {debounce} from 'debounce'

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

export class Scroller {
  triggers = []

  constructor(element) {
    this.element = element

    console.log(element)
    const onScroll = debounce(() => this.onScroll(), 100)
    const onReflow = debounce(() => this.update(), 200)

    window.addEventListener('resize', onReflow, {passive: true})
    this.element.addEventListener('scroll', onScroll)

    if (element instanceof Node) {
      this.observer = new MutationObserver(onReflow)
      this.observer.observe(element, {childList: true, subtree: false})
    }

    this.destroy = () => {
      window.removeEventListener('resize', onReflow, {passive: true})
      this.element.removeEventListener('scroll', onScroll)

      if (this.observer) this.observer.disconnect()
    }
  }

  get scrollY() {
    return this.element.scrollY
  }

  get scrollHeight() {
    return (
      (this.element.scrollHeight || document.body.scrollHeight) - this.height
    )
  }

  get progress() {
    return this.scrollY / this.scrollHeight
  }

  update() {
    this.height = this.element.offsetHeight || this.element.innerHeight
    this.triggers = this.triggers.map((trigger) => ({
      ...trigger,
      y: offsetTop(trigger.element)
    }))
    console.log(this)
  }

  addTrigger(element, onEnter) {
    const id = mkId()
    this.triggers.push({id, element, onEnter})
    this.update()
    return () => {
      const idx = this.triggers.findIndex((t) => t.id === id)
      this.triggers.splice(idx, 1)
    }
  }

  onScroll() {
    const threshold = this.scrollY + this.height / 2
    let i

    for (i = 0; i < this.triggers.length; ++i) {
      const trigger = this.triggers[i]
      if (trigger.y < threshold) break
    }

    const trigger = this.triggers[i - 1]

    if (!trigger || this.triggers.current === trigger.id) return

    requestIdleCallback(trigger.onEnter)
    this.triggers.current = trigger.id
  }
}
