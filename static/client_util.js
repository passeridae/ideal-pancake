
var getAllUsers = function(onSuccess, onError) {
    $.ajax(
        { url: '/users'
        , success: onSuccess
        , error: onError
        , type: 'GET'
        });
};
