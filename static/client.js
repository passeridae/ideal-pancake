
var postUsers = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/users'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getUsers = function(name, onSuccess, onError)
{
  $.ajax(
    { url: '/users' + '?name=' + encodeURIComponent(name)
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getUsersByUser_id = function(user_id, onSuccess, onError)
{
  $.ajax(
    { url: '/users/' + encodeURIComponent(user_id) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var postUsersByUser_id = function(user_id, onSuccess, onError)
{
  $.ajax(
    { url: '/users/' + encodeURIComponent(user_id) + ''
    , success: onSuccess
    , error: onError
    , type: 'POST'
    });
}

var postBooks = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/books'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getBooks = function(title, onSuccess, onError)
{
  $.ajax(
    { url: '/books' + '?title=' + encodeURIComponent(title)
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getBooksByBook_isbn = function(book_isbn, onSuccess, onError)
{
  $.ajax(
    { url: '/books/' + encodeURIComponent(book_isbn) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var postBooksByBook_isbn = function(book_isbn, onSuccess, onError)
{
  $.ajax(
    { url: '/books/' + encodeURIComponent(book_isbn) + ''
    , success: onSuccess
    , error: onError
    , type: 'POST'
    });
}

var postBooksByBook_isbnCopies = function(book_isbn, body, onSuccess, onError)
{
  $.ajax(
    { url: '/books/' + encodeURIComponent(book_isbn) + '/copies'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getBooksByBook_isbnCopies = function(book_isbn, onSuccess, onError)
{
  $.ajax(
    { url: '/books/' + encodeURIComponent(book_isbn) + '/copies'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getCopiesByCopy_id = function(copy_id, onSuccess, onError)
{
  $.ajax(
    { url: '/copies/' + encodeURIComponent(copy_id) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var postCopiesByCopy_id = function(copy_id, body, onSuccess, onError)
{
  $.ajax(
    { url: '/copies/' + encodeURIComponent(copy_id) + ''
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var deleteCopiesByCopy_id = function(copy_id, onSuccess, onError)
{
  $.ajax(
    { url: '/copies/' + encodeURIComponent(copy_id) + ''
    , success: onSuccess
    , error: onError
    , type: 'DELETE'
    });
}

var postRentals = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/rentals'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var postRentalsComplete = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/rentals/complete'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getRentalsByUser_id = function(user_id, onSuccess, onError)
{
  $.ajax(
    { url: '/rentals/' + encodeURIComponent(user_id) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
