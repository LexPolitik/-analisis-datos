use reqrnpdno::{extractora,parameters::Parametros};

fn main () {

    // Aquí se define la ruta en donde se guardarán los datos. En este caso es un archivo de tipo JSON llamado datos.json
    let rutam = "./datos.json".to_string();

    // Aquí creamos la estructura de los parámetros necesarios para realizar la petición. Esta estructura es la que se necesita modificar si quieres aplicar algún filtro. Si no quieres filtrar no es necesario modificar la estructura. 
    let mut parametros = Parametros::new();

    // Aquí se modifican los valores de los parámetros para aplicar algunos filtros. 
    // En este caso se coloca el valor "7" en el campo "id_estatus_victima" el cual corresponde a "PERSONAS DESAPARECIDAS Y NO LOCALIZADAS". 
    // También cambiamos el parámetro "titulo" y colocamos el título. 
    // Además usamos los parámetros "fecha_inicio" y "fecha_fin" para limitar la búsqueda a un rango de fechas.
    parametros.id_estatus_victima = "7".to_string();
    parametros.titulo = "PERSONAS DESAPARECIDAS Y NO LOCALIZADAS".to_string();
    parametros.fecha_inicio = "2024-01-01".to_string();
    parametros.fecha_fin = "2025-01-01".to_string();
    parametros.id_nacionalidad = "1".to_string(); // mexicana

    // Por último, utilizamos la función de alto nivel "extraer" para obtener nuestros datos.
    let ruta_salida = "./salida/".to_string();
    extractora::extraer_por_estados(&parametros, &ruta_salida).unwrap();
}

