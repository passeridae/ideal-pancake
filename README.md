## How to run

The server is run using `docker-compose`. To install docker run `curl -sSL https://get.docker.com/ | sh`. It may require root access

Once docker is installed run: `docker-compose up -d` The server is now serving at localhost on port 8080. To stop the server run `docker-compose down`

## How to build from source

If you wish to build from source there are a number of scripts to assist in construction of the docker image.

Run `./scripts/build_base.sh && ./scripts/build.sh` will build the base image, used for building as well as the smaller final image which runs the server

There should now be a new `oswynb/ideal-pancake` image on docker, which you can check by running `docker images`.
 You can then bring the server up as previously stated using `docker compose up -d`.

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
        "id": "3a30b665-e6a3-4856-a65c-153c8d41e305",
        "book_isbn": "9780060567231",
        "notes": "Damaged back cover"
    }
]
```

- 

```javascript
[
    {
        "id": "3a30b665-e6a3-4856-a65c-153c8d41e305",
        "book_isbn": "9780060567231",
        "notes": "Damaged back cover"
    },
    {
        "id": "8fe289e3-2adf-462a-a97f-64d72844d1c9",
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
    "id": "8f93590a-c357-4bd6-b543-f4358c2f9e05"
}
```

- Add copy success

```javascript
{
    "successful": true,
    "id": "2d2f3e47-3256-4d5b-ae32-de6c8f4da806"
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
    "id": "c253914f-0ca9-4a6b-95f3-247f7f2a7e56",
    "book_isbn": "9780060567231",
    "notes": "Damaged back cover"
}
```

- 

```javascript
{
    "id": "c253914f-0ca9-4a6b-95f3-247f7f2a7e56",
    "book_isbn": "9780060567231",
    "notes": "Coffee stains throughout"
}
```

- 

```javascript
{
    "id": "e3471fea-f2f8-4f9e-9b69-abb984da9d2e",
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
    "copy_id": "15bc95e4-48e4-4d47-8d38-316d0c1e6724",
    "user_id": "df0264d7-444e-4927-add9-bddf555489f6"
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
    "id": "1ebf14e3-1da0-4533-937b-fa553bed5437"
}
```

- Rental success

```javascript
{
    "successful": true,
    "id": "77d8f2de-13ea-4402-9ac7-ac93ac276f0d"
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
    "rental_id": "13a35070-e0ee-41c6-9fd4-39591e4cc362"
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
    "copy_id": "92db7122-222e-46ac-b7a9-f66cf9296c5a",
    "id": "375495c3-cf3b-447d-a407-7fbafba7bf90",
    "user_id": "3b8e1dce-147f-43e0-a7ea-c2f3199e8185"
}
```

- 

```javascript
{
    "due_date": "2016-11-30",
    "copy_id": "92db7122-222e-46ac-b7a9-f66cf9296c5a",
    "id": "375495c3-cf3b-447d-a407-7fbafba7bf90",
    "user_id": "3b8e1dce-147f-43e0-a7ea-c2f3199e8185"
}
```

- 

```javascript
{
    "due_date": "2016-11-30",
    "return_date": "2016-11-30",
    "copy_id": "92db7122-222e-46ac-b7a9-f66cf9296c5a",
    "id": "375495c3-cf3b-447d-a407-7fbafba7bf90",
    "user_id": "07dd7c9a-607a-471c-882a-c8bc9bdbd35e"
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
        "copy_id": "af04cb02-8d8b-4eb7-8383-51d0e2e6fc89",
        "id": "2a269d3f-9a0d-491b-b26d-469b00845c37",
        "user_id": "793586e6-4cc4-4c2e-b19d-756a1c9671f3"
    }
]
```

- 

```javascript
[
    {
        "due_date": "2016-11-30",
        "return_date": "2016-11-30",
        "copy_id": "af04cb02-8d8b-4eb7-8383-51d0e2e6fc89",
        "id": "2a269d3f-9a0d-491b-b26d-469b00845c37",
        "user_id": "793586e6-4cc4-4c2e-b19d-756a1c9671f3"
    },
    {
        "due_date": "2016-11-30",
        "return_date": "2016-11-30",
        "copy_id": "1297c80a-6a33-4e92-9f1d-fdcd9fc5e8c7",
        "id": "14f75db9-d8da-4a21-a93b-d33503511ffc",
        "user_id": "85860fba-daeb-4e0a-9026-813c7956bf36"
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
        "id": "de843781-f9f7-482e-b87f-6a0731928c4e"
    }
]
```

- 

```javascript
[
    {
        "name": "Oswyn Brent",
        "id": "de843781-f9f7-482e-b87f-6a0731928c4e"
    },
    {
        "name": "Oswyn Brent",
        "id": "a4328cbc-457e-41e5-8d86-7c688af6c3fd"
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
    "id": "1d12797e-7d42-4f7c-9473-736406241176"
}
```

- 

```javascript
{
    "id": "418f6f08-1da8-4a26-aa6e-9d60320e8c89"
}
```

- 

```javascript
{
    "id": "cdf3cd04-57c0-471f-a65f-95de78fbc695"
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
    "id": "2f39c235-76b9-4480-8cd6-2346f22608f1"
}
```

- 

```javascript
{
    "name": "Emily Olorin",
    "id": "2f39c235-76b9-4480-8cd6-2346f22608f1"
}
```

- 

```javascript
{
    "name": "Tristram Healy",
    "id": "2f39c235-76b9-4480-8cd6-2346f22608f1"
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

