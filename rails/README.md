# README

Guide: https://guides.rubyonrails.org/v5.1/getting_started.html#setting-the-application-home-page

## Notes
Controller receives requests to the application. Only public method are allowed in here.

Routing is decides which controller receives a request and there can be multiple routes to a single controller.

View displays the information to the user.

Resources is a term used for a collection of similar objects that you can perform CRUD operations on.

new.html.erb -> html is the format of the template while erb is the handler used to render the template

new.html.erb -> It is important to use redirect_to after mutating the database or application state as 
this causes the browser to make another request. If only using render, a user could refresh the page and the request is made again.

Partials are shared code between views where the file name is prepended by an underscore.
## Commands
rails generate controller UpperCaseName index

rails routes

rails generate model SingeUpperCaseName title:string text:text
- ActiveModel creates a migration with a new table and the attributes title and text

rails db:migrate