const path = require('path')
const flow = require('lodash/flow')

const ROOT = path.resolve(__dirname, '../..')

module.exports = () => (_, {addLoader, merge}) =>
  flow(
    merge({
      resolve: {extensions: ['.purs']}
    }),
    addLoader({
      test: /\.purs$/,
      include: [path.join(ROOT, 'src'), path.join(ROOT, '.spago')],
      use: [
        {
          loader: 'purs-loader',
          options: {
            spago: true
          }
        }
      ]
    })
  )
