### function to create gcs bucket
create_gcs_bucket <- function(bucket_name) {
  ### google authorization
  # It's often better to handle authentication outside of the function
  # to make the function more reusable.
  # Assuming authentication is handled before calling this function.
  # For example:
  # Sys.setenv(GCS_AUTH_FILE = "/path/to/your/gcs_auth.json")
  googleAuthR::gar_auth(scope = "https://www.googleapis.com/auth/cloud-platform",
                        json_file = Sys.getenv("GCS_AUTH_FILE"))
  
  tryCatch({
    # Check if the bucket already exists
    existing_buckets <- gcs_list_buckets()
    if (bucket_name %in% existing_buckets$name) {
      message(paste0("Bucket '", bucket_name, "' already exists."))
      return(invisible(NULL)) # Exit function gracefully
    }
    
    # Create the bucket if it doesn't exist
    gcs_create_bucket(bucket_name)
    
    # Print a success message
    message(paste0("Bucket '", bucket_name, "' created successfully."))
    
  }, error = function(e) {
    # Handle any errors that occur during the process
    message(paste0("An error occurred: ", e$message))
  })
}