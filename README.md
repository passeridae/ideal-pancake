## How to run


## How to build from source


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
        "id": "6e591b58-3843-4317-b1e9-2119893cc7c6",
        "book_isbn": "9780060567231",
        "notes": "Damaged back cover"
    }
]
```

- 

```javascript
[
    {
        "id": "6e591b58-3843-4317-b1e9-2119893cc7c6",
        "book_isbn": "9780060567231",
        "notes": "Damaged back cover"
    },
    {
        "id": "9e2c60fb-72f7-4b1c-a47b-1b6e5aba0a30",
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
    "id": "fcf66b73-99f9-4bba-b7e9-0faaeb02bfaf"
}
```

- Add copy success

```javascript
{
    "successful": true,
    "id": "d5a2a3da-0102-430c-9179-de77f63c9fc3"
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
    "id": "27dc4734-fe60-4b0d-9817-abcd72e67281",
    "book_isbn": "9780060567231",
    "notes": "Damaged back cover"
}
```

- 

```javascript
{
    "id": "27dc4734-fe60-4b0d-9817-abcd72e67281",
    "book_isbn": "9780060567231",
    "notes": "Coffee stains throughout"
}
```

- 

```javascript
{
    "id": "f07c39e3-1db3-44f2-8e67-37a0a3e52cf6",
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
    "copy_id": "66b09197-ca21-41ef-9632-778f81c3a4db",
    "user_id": "887a32de-9584-4516-8fbe-487deb8cbada"
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
    "id": "23735634-de4b-4e9c-9b51-fe3ec71322bc"
}
```

- Rental success

```javascript
{
    "successful": true,
    "id": "62d0b740-9451-492a-a79b-c72bde253818"
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
    "rental_id": "f9c935ba-f6f2-4a14-8e5d-7b9986ec6afe"
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
    "copy_id": "7fccd199-bff6-418a-a42c-5013d36c2105",
    "id": "ccbf03c8-23fe-4736-9949-bb319e6ab238",
    "user_id": "72c48e51-c707-401f-a891-379ef8ba50f5"
}
```

- 

```javascript
{
    "due_date": "2016-11-30",
    "copy_id": "7fccd199-bff6-418a-a42c-5013d36c2105",
    "id": "ccbf03c8-23fe-4736-9949-bb319e6ab238",
    "user_id": "72c48e51-c707-401f-a891-379ef8ba50f5"
}
```

- 

```javascript
{
    "due_date": "2016-11-30",
    "return_date": "2016-11-30",
    "copy_id": "7fccd199-bff6-418a-a42c-5013d36c2105",
    "id": "ccbf03c8-23fe-4736-9949-bb319e6ab238",
    "user_id": "84d2b90c-41ae-4a8a-b03e-367f6d856c2a"
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
        "copy_id": "cdeaf270-747a-44cd-a044-8978205ce24c",
        "id": "ca25373c-dcdf-4a99-bce7-31253b3c3bf3",
        "user_id": "b2d74671-74d3-4956-af62-3c8d289576e0"
    }
]
```

- 

```javascript
[
    {
        "due_date": "2016-11-30",
        "return_date": "2016-11-30",
        "copy_id": "cdeaf270-747a-44cd-a044-8978205ce24c",
        "id": "ca25373c-dcdf-4a99-bce7-31253b3c3bf3",
        "user_id": "b2d74671-74d3-4956-af62-3c8d289576e0"
    },
    {
        "due_date": "2016-11-30",
        "return_date": "2016-11-30",
        "copy_id": "a46558e0-aea0-41e9-baab-7c41a176c783",
        "id": "f6a8d1a5-f032-444e-b42f-5ed840354582",
        "user_id": "3b082548-a340-4ab2-b451-efe6a0df422c"
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
        "id": "653e24ea-1840-48f8-bb41-664ed6353fff"
    }
]
```

- 

```javascript
[
    {
        "name": "Oswyn Brent",
        "id": "653e24ea-1840-48f8-bb41-664ed6353fff"
    },
    {
        "name": "Oswyn Brent",
        "id": "5758c907-9c59-492d-9692-618f68811662"
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
    "id": "c3f9d4b7-062d-4f04-808e-eae805ecb311"
}
```

- 

```javascript
{
    "id": "4e176eae-6d79-4dab-bdfa-d9d0fb855815"
}
```

- 

```javascript
{
    "id": "8738f1fd-5e3e-4772-8f38-f766ce931a1d"
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
    "id": "865f54c0-49b6-42ee-8dea-0b7a96202a4f"
}
```

- 

```javascript
{
    "name": "Emily Olorin",
    "id": "865f54c0-49b6-42ee-8dea-0b7a96202a4f"
}
```

- 

```javascript
{
    "name": "Tristram Healy",
    "id": "865f54c0-49b6-42ee-8dea-0b7a96202a4f"
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

