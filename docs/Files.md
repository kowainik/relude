Files
=====

Basic IO
--------

#### readFile

```haskell
readFile :: FilePath -> IO Text
```

#### writeFile

```haskell
writeFile :: FilePath -> Text -> IO ()
```

#### appendFile

```haskell
appendFile :: FilePath -> Text -> IO ()
```

Console
-------

#### getLine

```haskell
getLine :: IO Text
```

#### getContents

```haskell
getContents :: IO Text
```

#### interact

```haskell
interact :: (Text -> Text) -> IO ()
```

File Handles
------------

```haskell
data IOMode 
  = ReadMode
  | WriteMode
  | AppendMode
  | ReadWriteMode
```

#### openFile

```haskell
openFile :: FilePath -> IOMode -> IO Handle
```

#### withFile

```haskell
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
```


#### stdin
#### stdout
#### stderr
#### Handle
#### FilePath
