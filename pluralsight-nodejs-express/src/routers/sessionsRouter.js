const express = require('express')
const sessionsRouter = express.Router()
const sessions = require('../data/sessions.json')

// Setup routing. For router, the base URL from app.use is already inherited, so
// there is no need to include it in the route
sessionsRouter.route('/')
    .get((req, res) => {
        res.render('sessions', {
            // Pulls all sessions from sessions.json data file
            sessions
        })
    })

// '/:PARAM'
sessionsRouter.route('/:id')
    .get((req, res) => {
        const id = req.params.id;
        res.render('session', {
            session: sessions[id]
        });
    });

// Export sessionsRouter for app.js
module.exports = sessionsRouter;