module.exports = {
  env: {
    browser: true,
    commonjs: true,
    node: true,
    es6: true
  },
  extends: ['prettier', 'prettier/react', 'eslint:recommended'],
  parser: 'babel-eslint',
  parserOptions: {
    ecmaFeatures: {
      experimentalObjectRestSpread: true,
      jsx: true
    },
    ecmaVersion: 2020,
    sourceType: 'module'
  },
  overrides: [
    {
      files: ['*.ts', '*.tsx'],
      plugins: ['react', 'prettier', '@typescript-eslint'],
      extends: [
        'prettier',
        'prettier/react',
        'prettier/@typescript-eslint',
        'eslint:recommended',
        'plugin:@typescript-eslint/recommended'
      ],
      parser: '@typescript-eslint/parser',
      parserOptions: {
        ecmaFeatures: {
          experimentalObjectRestSpread: true,
          jsx: true
        },
        ecmaVersion: 2020,
        sourceType: 'module'
      }
    }
  ],
  rules: {
    'prettier/prettier': [
      'error',
      {
        bracketSpacing: false,
        singleQuote: true,
        semi: false,
        arrowParens: 'always',
        trailingComma: 'none'
      }
    ],
    'array-bracket-spacing': [2, 'never'],
    'comma-dangle': [
      'error',
      {
        arrays: 'ignore',
        objects: 'never',
        imports: 'never',
        exports: 'never',
        functions: 'ignore'
      }
    ],
    'linebreak-style': ['error', 'unix'],
    quotes: ['error', 'single'],
    'react/jsx-uses-vars': 1,
    semi: ['error', 'never'],
    'no-unused-vars': ['error', {argsIgnorePattern: '^_'}],
    'object-curly-spacing': ['error', 'never']
  },
  globals: {
    process: true
  }
}
