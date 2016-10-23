## GET /books

#### Authentication



Clients must supply the following data


#### GET Parameters:

- title
     - **Values**: **
     - **Description**: title or title fragment to search for


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[
    {
        "authors": [
            "Emily Olorin",
            "Oswyn Brent"
        ],
        "isbn": "lol-legit-isbn",
        "publishers": [
            "Sadness Publishing"
        ],
        "title": "A Story of Sadness",
        "date_of_publication": "2016-09-30"
    }
]
```

- 

```javascript
[
    {
        "authors": [
            "Emily Olorin",
            "Oswyn Brent"
        ],
        "isbn": "lol-legit-isbn",
        "publishers": [
            "Sadness Publishing"
        ],
        "title": "A Story of Sadness",
        "date_of_publication": "2016-09-30"
    },
    {
        "authors": [
            "Emily Olorin",
            "Oswyn Brent"
        ],
        "isbn": "lol-legit-isbn",
        "publishers": [
            "Sadness Publishing"
        ],
        "title": "A Story of Sadness",
        "date_of_publication": "2016-09-30"
    }
]
```

## POST /books

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "authors": [
        "Emily Olorin",
        "Oswyn Brent"
    ],
    "isbn": "lol-legit-isbn",
    "publishers": [
        "Sadness Publishing"
    ],
    "title": "A Story of Sadness",
    "date_of_publication": "2016-09-30"
}
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## GET /books/:book_isbn

#### Authentication



Clients must supply the following data


#### Captures:

- *book_isbn*: isbn of the book

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
{
    "authors": [
        "Emily Olorin",
        "Oswyn Brent"
    ],
    "isbn": "lol-legit-isbn",
    "publishers": [
        "Sadness Publishing"
    ],
    "title": "A Story of Sadness",
    "date_of_publication": "2016-09-30"
}
```

## POST /books/:book_isbn

#### Authentication



Clients must supply the following data


#### Captures:

- *book_isbn*: isbn of the book

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## GET /books/:book_isbn/copies

#### Authentication



Clients must supply the following data


#### Captures:

- *book_isbn*: isbn of the book

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[
    {
        "id": "3180f6a1-ce9a-47f6-9594-2524d227a07f",
        "book_isbn": "9780060567231",
        "notes": "Damaged back cover"
    }
]
```

- 

```javascript
[
    {
        "id": "3180f6a1-ce9a-47f6-9594-2524d227a07f",
        "book_isbn": "9780060567231",
        "notes": "Damaged back cover"
    },
    {
        "id": "c77c9295-d1c8-4620-be99-beffc91444f0",
        "book_isbn": "9780060567231",
        "notes": "Damaged back cover"
    }
]
```

## POST /books/:book_isbn/copies

#### Authentication



Clients must supply the following data


#### Captures:

- *book_isbn*: isbn of the book

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "notes": "Damaged back cover"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Book doesn't exist within library

```javascript
{
    "successful": false
}
```

- Add copy success

```javascript
{
    "successful": true,
    "id": "ecf774c9-7c0d-4442-b921-1b28d574b321"
}
```

- Add copy success

```javascript
{
    "successful": true,
    "id": "70629fc2-f10d-44a3-875a-1a541d177ceb"
}
```

## DELETE /copies/:copy_id

#### Authentication



Clients must supply the following data


#### Captures:

- *copy_id*: unique identifier for the book copy

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## GET /copies/:copy_id

#### Authentication



Clients must supply the following data


#### Captures:

- *copy_id*: unique identifier for the book copy

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "id": "6ad379ce-b59b-4348-9151-bbbabccb2b82",
    "book_isbn": "9780060567231",
    "notes": "Damaged back cover"
}
```

- 

```javascript
{
    "id": "6ad379ce-b59b-4348-9151-bbbabccb2b82",
    "book_isbn": "9780060567231",
    "notes": "Coffee stains throughout"
}
```

- 

```javascript
{
    "id": "7e15599f-3200-4ad8-973b-43c61448bbf9",
    "book_isbn": "9780060567231",
    "notes": "Damaged back cover"
}
```

## POST /copies/:copy_id

#### Authentication



Clients must supply the following data


#### Captures:

- *copy_id*: unique identifier for the book copy

#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "notes": "Damaged back cover"
}
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

## POST /rentals

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "due_date": "2016-11-30",
    "copy_id": "3070cdf7-a4a1-4a0b-b123-062549f61117",
    "user_id": "9a514db1-6302-41b5-8875-9a27829b6de8"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Copy already on loan

```javascript
{
    "successful": false
}
```

- Rental success

```javascript
{
    "successful": true,
    "id": "3ddbee43-2bd8-45a3-94f7-423d60cdfc34"
}
```

- Rental success

```javascript
{
    "successful": true,
    "id": "fac59fbf-9d46-4de5-8494-70485fcd6302"
}
```

## POST /rentals/complete

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "rental_id": "5a19e701-48c9-4a88-bd92-747051123ffa"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Rental doesn't exist

```javascript
{
    "successful": false,
    "message": "Rental doesn't exist"
}
```

- Rental is already complete

```javascript
{
    "successful": false,
    "message": "Rental is already complete"
}
```

- Complete rental success

```javascript
{
    "successful": true,
    "message": "Complete rental success"
}
```

## GET /rentals/copy/:copy_id

#### Authentication



Clients must supply the following data


#### Captures:

- *copy_id*: unique identifier for the book copy

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "due_date": "2016-11-30",
    "return_date": "2016-11-30",
    "copy_id": "c9eb64a9-f339-4317-abec-1f0abb3cc262",
    "id": "b96ddec1-1ee6-42df-aa71-03e3b4167d94",
    "user_id": "24807aef-5977-41e8-bf5b-63de44235507"
}
```

- 

```javascript
{
    "due_date": "2016-11-30",
    "copy_id": "c9eb64a9-f339-4317-abec-1f0abb3cc262",
    "id": "b96ddec1-1ee6-42df-aa71-03e3b4167d94",
    "user_id": "24807aef-5977-41e8-bf5b-63de44235507"
}
```

- 

```javascript
{
    "due_date": "2016-11-30",
    "return_date": "2016-11-30",
    "copy_id": "c9eb64a9-f339-4317-abec-1f0abb3cc262",
    "id": "b96ddec1-1ee6-42df-aa71-03e3b4167d94",
    "user_id": "ae4efbfa-8b73-49fd-a44b-ed91b44a5bf4"
}
```

## GET /rentals/user/:user_id

#### Authentication



Clients must supply the following data


#### Captures:

- *user_id*: unique identifier for the user

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[
    {
        "due_date": "2016-11-30",
        "return_date": "2016-11-30",
        "copy_id": "e8c1ec5c-caea-4963-b04f-9ddf6194aebf",
        "id": "0140182a-2c52-4ac8-9d62-a749d372d75b",
        "user_id": "29b49da6-dfeb-4427-8127-b26b093daf2b"
    }
]
```

- 

```javascript
[
    {
        "due_date": "2016-11-30",
        "return_date": "2016-11-30",
        "copy_id": "e8c1ec5c-caea-4963-b04f-9ddf6194aebf",
        "id": "0140182a-2c52-4ac8-9d62-a749d372d75b",
        "user_id": "29b49da6-dfeb-4427-8127-b26b093daf2b"
    },
    {
        "due_date": "2016-11-30",
        "return_date": "2016-11-30",
        "copy_id": "b16b095a-b72f-443d-989a-abc1a7114700",
        "id": "ee56637c-b77c-4233-9507-909aa42b6336",
        "user_id": "887c779b-e65a-4041-96da-65da17816b0e"
    }
]
```

## GET /users

#### Authentication



Clients must supply the following data


#### GET Parameters:

- name
     - **Values**: **
     - **Description**: name or name fragment to search for


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
[]
```

- 

```javascript
[
    {
        "name": "Oswyn Brent",
        "id": "271e5fb4-8e0b-4a96-b8f2-17a7f53d5826"
    }
]
```

- 

```javascript
[
    {
        "name": "Oswyn Brent",
        "id": "271e5fb4-8e0b-4a96-b8f2-17a7f53d5826"
    },
    {
        "name": "Oswyn Brent",
        "id": "0bfc1f4b-e2a2-4e4f-aa3a-d95cf3d1bb20"
    }
]
```

## POST /users

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json`

- Example: `application/json`

```javascript
{
    "name": "Oswyn Brent"
}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "id": "0f185427-fa89-4f30-aae4-47fdedb983d4"
}
```

- 

```javascript
{
    "id": "40c1cc24-42fa-440f-845e-322205529af1"
}
```

- 

```javascript
{
    "id": "02fcb7ba-043b-40b7-a16e-86d6eb0d126a"
}
```

## GET /users/:user_id

#### Authentication



Clients must supply the following data


#### Captures:

- *user_id*: unique identifier for the user

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- 

```javascript
{
    "name": "Oswyn Brent",
    "id": "803c7b5a-c0a8-4405-8bac-9fb43b0e8595"
}
```

- 

```javascript
{
    "name": "Emily Olorin",
    "id": "803c7b5a-c0a8-4405-8bac-9fb43b0e8595"
}
```

- 

```javascript
{
    "name": "Tristram Healy",
    "id": "803c7b5a-c0a8-4405-8bac-9fb43b0e8595"
}
```

## POST /users/:user_id

#### Authentication



Clients must supply the following data


#### Captures:

- *user_id*: unique identifier for the user

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript

```

