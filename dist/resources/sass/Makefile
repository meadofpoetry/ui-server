SASS = sassc
POSTCSS = postcss -u autoprefixer -r
BUILD_DIR = ../css

%.min.css : %.scss
	$(SASS) -t compressed -I ./ $< $@
	$(POSTCSS) $@
	mv $@ $(BUILD_DIR)/$(@F)

all: components main boards pages

main: ./main.min.css

components: ./@material/components/components.min.css \
						./@material-lab/components-lab/components-lab.min.css

boards: ./frontend/boards/board-niitv-dvb4ch/board-niitv-dvb4ch.min.css \
				./frontend/boards/board-niitv-tsan/board-niitv-tsan.min.css

pages:  ./frontend/page-input/page-input.min.css \
				./frontend/page-mosaic-editor/page-mosaic-editor.min.css \
				./frontend/page-mosaic-video/page-mosaic-video.min.css \
				./frontend/page-topology/page-topology.min.css \
				./frontend/page-network-settings/page-network-settings.min.css \
				./frontend/page-software-updates/page-software-updates.min.css \
				./frontend/page-power-management/page-power-management.min.css \
				./frontend/page-server-settings/page-server-settings.min.css \
				./frontend/page-user-settings/page-user-settings.min.css \
				./frontend/page-error/page-error.min.css \
				./frontend/page-logout/page-logout.min.css \
				./frontend/boards/board-niitv-dvb4ch/page-input/board-niitv-dvb4ch-page-input.min.css \
				./frontend/boards/board-niitv-tsan/page-input/board-niitv-tsan-page-input.min.css

.PHONY: all main components pages
