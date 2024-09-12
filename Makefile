# Variables for convenience
IMAGE_NAME=unhoused_report
CONTAINER_NAME=unhoused_report_container
R_SCRIPT_DIR=processing/src
TIMESTAMP=.build_time

# Build the Docker image, only if Dockerfile has changed or the timestamp file doesn't exist
build: Dockerfile
	@if [ ! -f $(TIMESTAMP) ] || [ Dockerfile -nt $(TIMESTAMP) ]; then \
		echo "Building the Docker image..."; \
		docker build -t $(IMAGE_NAME) . && touch $(TIMESTAMP); \
	else \
		echo "Docker image is up to date."; \
	fi

# Start the container if it doesn't exist or is stopped
start-container: build
	@if [ ! "$(shell docker ps --filter name=$(CONTAINER_NAME) --filter status=running -q)" ]; then \
		if [ "$(shell docker ps -a --filter name=$(CONTAINER_NAME) -q)" ]; then \
			echo "Starting existing container $(CONTAINER_NAME)..."; \
			docker start $(CONTAINER_NAME); \
		else \
			echo "Creating and starting a new container $(CONTAINER_NAME)..."; \
			docker run -d --name $(CONTAINER_NAME) -v $(shell pwd):/app $(IMAGE_NAME); \
		fi \
	else \
		echo "Container $(CONTAINER_NAME) is already running."; \
	fi

# Run an R script inside the container (assuming the container is running)
run-rscript: start-container
	@echo "Running the specified R script inside the container..."
	docker exec $(CONTAINER_NAME) Rscript /app/$(R_SCRIPT)

# Specific targets to run each R script
run-lahsa: R_SCRIPT=$(R_SCRIPT_DIR)/processing_lahsa.R
run-lahsa: run-rscript

run-lapd: R_SCRIPT=$(R_SCRIPT_DIR)/processing_lapd.R
run-lapd: run-rscript

run-lasan: R_SCRIPT=$(R_SCRIPT_DIR)/processing_lasan.R
run-lasan: run-rscript

run-other: R_SCRIPT=$(R_SCRIPT_DIR)/processing_other.R
run-other: run-rscript

run-census: R_SCRIPT=$(R_SCRIPT_DIR)/tidy_census_processing.R
run-census: run-rscript

# Run all R scripts sequentially
all: run-lahsa run-lasan run-other run-census run-lapd

# Stop the container if needed (optional)
stop-container:
	@if [ "$(shell docker ps -q -f name=$(CONTAINER_NAME))" ]; then \
		echo "Stopping container $(CONTAINER_NAME)..."; \
		docker stop $(CONTAINER_NAME); \
	else \
		echo "Container $(CONTAINER_NAME) is not running."; \
	fi

# Clean up dangling Docker images and stopped containers
clean:
	@echo "Cleaning up unused Docker containers and images..."
	docker system prune -f
	@rm -f $(TIMESTAMP)

# Help: Display available targets
help:
	@echo "Available commands:"
	@echo "  make build       - Build the Docker image"
	@echo "  make start-container - Start or create the Docker container"
	@echo "  make stop-container - Stop the running Docker container"
	@echo "  make run-lahsa   - Run processing_lahsa.R"
	@echo "  make run-lapd    - Run processing_lapd.R"
	@echo "  make run-lasan   - Run processing_lasan.R"
	@echo "  make run-other   - Run processing_other.R"
	@echo "  make run-census  - Run tidy_census_processing.R"
	@echo "  make all         - Run all processing R scripts sequentially"
	@echo "  make clean       - Clean up unused Docker containers and images"

