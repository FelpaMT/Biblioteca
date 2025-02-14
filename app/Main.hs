import Data.List
import Data.List.Split (splitOn)
import System.IO

data Libro = Libro {
    nombre :: String,
    idLibro :: Int,
    estado :: String,
    genero :: String,
    prestamo :: String,
    clasificacionEdad :: String,
    fechaPrestamo :: String,
    cliente :: String
} deriving (Show, Eq)

-- Función para leer y parsear el CSV
leerCSV :: FilePath -> IO [Libro]
leerCSV ruta = do
    contenido <- readFile ruta
    let filas = lines contenido
    return $ map parsearLibro (tail filas) 
  where
    parsearLibro :: String -> Libro
    parsearLibro linea = case splitOn "," linea of
        [n, i, e, g, p, ce, fp, c] -> Libro n (read i) e g p ce fp c
        _ -> error "Formato de CSV inválido"

-- Función para filtrar libros por clasificación de edad
librosPorEdad :: String -> [Libro] -> [Libro]
librosPorEdad edad = filter ((== edad) . clasificacionEdad)

-- Función para filtrar libros prestados por género
librosPorGenero :: String -> [Libro] -> [Libro]
librosPorGenero g = filter (\l -> genero l == g && (prestamo l == "SI" || prestamo l == "si"))

-- Función para obtener libros pendientes por devolución
librosPendientes :: [Libro] -> [Libro]
librosPendientes = filter ((== "NO") . prestamo)

-- Función para filtrar libros por estado
librosPorEstado :: String -> [Libro] -> [Libro]
librosPorEstado est = filter ((== est) . estado)

-- Cantidad total de préstamos
cantidadPrestamos :: [Libro] -> Int
cantidadPrestamos = length . filter (\l -> prestamo l == "SI" || prestamo l == "si")

-- Imprimir tabla con encabezados y datos
imprimirTabla :: [String] -> [[String]] -> IO ()
imprimirTabla encabezados filas = do
    putStrLn $ intercalate " | " encabezados
    putStrLn $ replicate (length (intercalate " | " encabezados)) '-'
    mapM_ (putStrLn . intercalate " | ") filas

-- Menú de consultas para seleccionar la consulta a realizar
menuConsultas :: [Libro] -> IO ()
menuConsultas libros = do
    putStrLn "\nSeleccione la consulta que desea realizar:"
    putStrLn "1. Libros prestados por clasificación de edad"
    putStrLn "2. Libros prestados por género"
    putStrLn "3. Libros pendientes por devolución"
    putStrLn "4. Libros por estado"
    putStrLn "5. Cantidad total de préstamos"
    putStrLn "6. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la clasificación de edad (Posibles: '0-12', '14-16', '18-99'):"
            clasificacionFiltro <- getLine
            imprimirTabla 
                ["Nombre", "Id", "Estado", "Género", "Préstamo", "Clasificación", "Fecha Préstamo", "Cliente"]
                (map (\l -> [nombre l, show (idLibro l), estado l, genero l, prestamo l, clasificacionEdad l, fechaPrestamo l, cliente l]) 
                     (librosPorEdad clasificacionFiltro libros))
            menuConsultas libros
        "2" -> do
            putStrLn "Ingrese el genero de los libros prestados que desea consultar (Posibles: 'Epopeya', 'Novela', 'Fabula', 'Aventura', 'Fantasia', 'Realismo Magico', 'Misterio', 'Biografia', 'Novela Historica', 'Autobiografia', 'Religion', 'Drama', 'Ficcion'): "
            generoFiltro <- getLine
            imprimirTabla 
                ["Nombre", "Id", "Clasificación Edad", "Fecha Préstamo", "Cliente"]
                (map (\l -> [nombre l, show (idLibro l), clasificacionEdad l, fechaPrestamo l, cliente l]) 
                     (librosPorGenero generoFiltro libros))
            menuConsultas libros
        "3" -> do
            putStrLn "\nLibros pendientes por devolución:"
            imprimirTabla 
                ["Nombre", "Id", "Estado", "Género", "Préstamo", "Clasificación Edad", "Fecha Préstamo", "Cliente"]
                (map (\l -> [nombre l, show (idLibro l), estado l, genero l, prestamo l, clasificacionEdad l, fechaPrestamo l, cliente l]) 
                     (librosPendientes libros))
            menuConsultas libros
        "4" -> do
            putStrLn "Ingrese el estado de los libros (Posibles: 'Bueno', 'Regular', 'Malo'):"
            estadoFiltro <- getLine
            imprimirTabla 
                ["Nombre", "Id", "Estado"]
                (map (\l -> [nombre l, show (idLibro l), estado l]) 
                     (librosPorEstado estadoFiltro libros))
            menuConsultas libros
        "5" -> do
            putStrLn "\nCantidad total de préstamos:"
            print (cantidadPrestamos libros)
            menuConsultas libros
        "6" -> putStrLn "Saliendo..."
        _   -> do
            putStrLn "Opción inválida, intente nuevamente."
            menuConsultas libros

main :: IO ()
main = do
    libros <- leerCSV "libros.csv"
    menuConsultas libros
