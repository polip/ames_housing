### function to create gcs bucket
create_gcs_bucket <- function(bucket_name) {

  ### google authorization
  Sys.setenv()(GCS_AUTH_FILE = "/Users/ivan/Documents/gcs_auth.json")
  googleAuthR::gar_auth(scope = "https://www.googleapis.com/auth/cloud-platform",
                        json_file = Sys.getenv("GCS_AUTH_FILE"))

  # Create the bucket
  gcs_create_bucket(bucket_name)

  # Print a success message
  message(paste0("Bucket '", bucket_name, "' created successfully."))

}### function to create gcs bucket
create_gcs_bucket <- function(bucket_name) {

  ### google authorization
  Sys.setenv()
}