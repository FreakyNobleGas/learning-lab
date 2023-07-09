// Set up Express. Everything works through app in express
const express = require('express')
const PORT = process.env.PORT || 3000;
const app = express();

// Setup debugging tools
const chalk = require('chalk');
const debug = require('debug')('app');
const morgan = require('morgan')

// Setup morgan to log web traffic
app.use(morgan('tiny'))

// Serve static HTML files in the public function. This function call explicitly looks for
// a file called 'index.html'. (Which is intentionally missing so we can call ejs files below)
const path = require('path');
app.use(express.static(path.join(__dirname, '/public/')));

// Setup view engine with ejs
app.set('views', './src/views');
app.set('view engine', 'ejs');

// Base URL for app, this uses index.ejs since we are using res.render()
app.get('/', (req, res)=>{
    // Pass variable to ejs and render website
    res.render('index', { title: 'Globomantics', data: ['a', 'b', 'c'] });
});

// Sessions Router is defined in ./src/routers/sessionsRouter.js
const sessionsRouter = require('./src/routers/sessionsRouter')
app.use('/sessions', sessionsRouter)

// Bind app to port
app.listen(PORT, ()=> {
    // Template String. Debug is used in place of console.log, which only runs in debug mode!
    debug(`listening to port ${chalk.green(PORT)}`);
})