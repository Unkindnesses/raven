const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');

module.exports = function(context, options) {
    return {
        name: 'monaco-plugin',
        configureWebpack(config, isServer, utils) {
            config.module.rules.push(
                {test: /\.ttf$/, use: ['file-loader']}
            );
            config.plugins.push(new MonacoWebpackPlugin());
            return {};
        }
    }
}
