CC = sassc -t compressed
BUILD_DIR = ../css

all: components main pages

main:
	$(CC) -I ./ ./main.scss ../css/main.min.css

components:
	$(CC) -I ./ ./@material/components/components.scss $(BUILD_DIR)/components.min.css
	$(CC) -I ./ ./@material-lab/components-lab/components-lab.scss $(BUILD_DIR)/components-lab.min.css

pages:
	$(CC) -I ./ ./frontend/input/input.scss $(BUILD_DIR)/page-input.min.css
	$(CC) -I ./ ./frontend/mosaic-editor/mosaic-editor.scss $(BUILD_DIR)/page-mosaic-editor.min.css
	$(CC) -I ./ ./frontend/mosaic-video/mosaic-video.scss $(BUILD_DIR)/page-mosaic-video.min.css
	$(CC) -I ./ ./frontend/topology/topology.scss $(BUILD_DIR)/page-topology.min.css
	$(CC) -I ./ ./frontend/network-settings/network-settings.scss $(BUILD_DIR)/page-network-settings.min.css
	$(CC) -I ./ ./frontend/software-updates/software-updates.scss $(BUILD_DIR)/page-software-updates.min.css
	$(CC) -I ./ ./frontend/power-management/power-management.scss $(BUILD_DIR)/page-power-management.min.css
	$(CC) -I ./ ./frontend/server-settings/server-settings.scss $(BUILD_DIR)/page-server-settings.min.css
	$(CC) -I ./ ./frontend/user-settings/user-settings.scss $(BUILD_DIR)/page-user-settings.min.css
	$(CC) -I ./ ./frontend/error/error.scss $(BUILD_DIR)/page-error.min.css
	$(CC) -I ./ ./frontend/logout/logout.scss $(BUILD_DIR)/page-logout.min.css
	$(CC) -I ./ ./frontend/boards/niitv-dvb4ch/niitv-dvb4ch.scss $(BUILD_DIR)/board-niitv-dvb4ch.min.css
	$(CC) -I ./ ./frontend/boards/niitv-dvb4ch/input/input.scss $(BUILD_DIR)/board-niitv-dvb4ch-page-input.min.css
	$(CC) -I ./ ./frontend/boards/niitv-tsan/niitv-tsan.scss $(BUILD_DIR)/board-niitv-tsan.min.css
	$(CC) -I ./ ./frontend/boards/niitv-tsan/input/input.scss $(BUILD_DIR)/board-niitv-tsan-page-input.min.css

.PHONY: all main components pages
