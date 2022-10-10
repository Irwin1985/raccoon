## Raccoon

`Raccoon` it's a toy language inspired from `dBase` and `Visual FoxPro`. It's goal is to provide a collection of commands for accessing any remote database and manipulate the data without using `SQL`.

Examples:

```xBase
// First we need to create a connection object
CREATE CONNECTION loCon

// Now let's set the remote database parameters
loCon.provider = _SQL_SERVER // this is a constant value.
loCon.server = "127.0.0.1"
loCon.user = "sa"
loCon.password = "1234"
loCon.database = "Northwind"

// Connect to remote DB
SET CONNECTION TO loCon OR MESSAGEBOX('Failed to connect!')

// Open a table in readonly mode
USE Customers READONLY

// Iterate all rows
SELECT Customers
SCAN
  MESSAGEBOX(Customers.Name)
ENDSCAN

// Close cursor
USE IN Customers

// Close database
CLOSE loCon

```