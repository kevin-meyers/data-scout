module DataStructures where

data Permission = View | Edit | Own

data TableAccess = TableAccess { 
    table :: Table,
    permission :: Permission
}

type AccessableTables = [TableAccess]

data Table = Table {
    name :: String,
    owningTeam :: Team,
    columns :: [Column],
    description :: String
}

data Team = Team {
    name :: String,
    description :: String,
    contact :: ContactInfo
}

data ContactInfo = ContactInfo {
    phone :: Maybe PhoneNumber,
    email :: Maybe EmailAddress
}

type PhoneNumber = String

type EmailAddress = String

data Column = Column {
    name :: String,
    description :: String,
    connections :: [Connection],
    dataType :: DataType,
    example :: String
}

data Connection = Connection {
    table :: Table,
    column :: Column
}

data DataType = Text | Numeric | Integer | Real | Blob
