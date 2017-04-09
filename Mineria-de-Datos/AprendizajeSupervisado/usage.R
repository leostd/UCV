
# Seleccionar google_api.R en su sistema de archivos
source(file.choose())

origen = c("Via Paolo Emilio", "Vancouver BC", "Seattle")
destino =c("Piazzale Aldo Moro", "San Francisco", "Victoria BC")

# Colocar su API Key 
api_key = "AIzaSyCCcqG81xzm1jWcgRs095rx7s913eOiQvc"

api_url = get_url(origen, destino, api_key)

datos = get_data(api_url)
datos["rows"]$rows$elements
