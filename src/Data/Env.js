'use strict'

exports.getOptions = () => ({
  github: {token: atob(process.env.GH_TOKEN)}
})
