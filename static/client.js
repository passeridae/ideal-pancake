
var getUsers = function(onSuccess, onError)
{
  $.ajax(
    { url: '/users'
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

var getBooks = function(onSuccess, onError)
{
  $.ajax(
    { url: '/books'
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
