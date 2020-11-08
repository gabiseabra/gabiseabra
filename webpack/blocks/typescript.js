const path = require('path')
const flow = require('lodash/flow')

const ROOT = path.resolve(__dirname, '../..')

module.exports = () => (_, {addLoader, merge}) =>
  flow(
    merge({
      resolve: {extensions: ['.ts', '.tsx']}
    }),
    addLoader({
      test: /\.tsx?$/,
      include: [path.join(ROOT, 'src')],
      use: [
        {
          loader: 'babel-loader',
          options: {babelrc: true, cacheDirectory: true}
        },
        {loader: 'ts-loader', options: {transpileOnly: true}}
      ]
    })
  )
