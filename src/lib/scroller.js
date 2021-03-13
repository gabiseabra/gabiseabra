const rIC = (fn) => {
  let id
  return (e) => {
    if (id) cancelIdleCallback(id)
    id = requestIdleCallback(() => fn(e))
  }
}

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

    const onScroll = rIC(() => this.onScroll())
    const onReflow = rIC(() => this.update())

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
    return this.element.scrollTop
  }

  get scrollHeight() {
    return this.element.scrollHeight - this.height
  }

  get progress() {
    return this.scrollY / this.scrollHeight
  }

  update() {
    this.height = this.element.offsetHeight
    this.triggers = this.triggers.map((trigger) => ({
      ...trigger,
      y: offsetTop(trigger.element)
    }))
  }

  addTrigger({id, element, onEnter}) {
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
      if (trigger.y > threshold) break
    }

    const trigger = this.triggers[i - 1]

    if (!trigger || this.triggers.current === trigger.id) return

    this.triggers.current = trigger.id

    requestIdleCallback(() => this.updateTrigger())
  }

  updateTrigger() {
    /* TODO */
  }
}
