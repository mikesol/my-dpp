import TypeIt from 'typeit'

export const typeitGoImpl = (e) => (options) => () => {e.innerHTML = '';typeit(e)(options)().go()}

export const typeit = (e) => (options) => () =>
  new TypeIt(e, {
    ...options,
    afterComplete: options.afterComplete
      ? (i) => options.afterComplete(i)()
      : () => {},
    beforeStep: options.beforeStep ? (i) => options.beforeStep(i)() : () => {},
    beforeString: options.beforeString
      ? (c, i) => options.beforeString(i)(c)()
      : () => {},
    afterStep: options.afterStep ? (i) => options.afterStep(i)() : () => {},
    afterString: options.afterString
      ? (c, i) => options.afterString(i)(c)()
      : () => {},
  })

export const go = (ti) => () => ti.go()

export const destroy = (ti) => (removeCursor) => () => ti.destroy(removeCursor)
