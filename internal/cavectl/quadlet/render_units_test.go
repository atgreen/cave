package quadlet

import (
	"os"
	"path/filepath"
	"testing"

	"moxielogic.com/cave/internal/cavectl/config"
)

// TestRenderUnitsForInspection writes the generated units to CAVECTL_RENDER_DIR
// for a config given by CAVECTL_RENDER_CONFIG. Skipped unless both are set.
func TestRenderUnitsForInspection(t *testing.T) {
	src := os.Getenv("CAVECTL_RENDER_CONFIG")
	dst := os.Getenv("CAVECTL_RENDER_DIR")
	if src == "" || dst == "" {
		t.Skip("set CAVECTL_RENDER_CONFIG and CAVECTL_RENDER_DIR")
	}
	cfg, err := config.Load(src)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(dst, 0o755); err != nil {
		t.Fatal(err)
	}
	for name, body := range generate(cfg) {
		if err := os.WriteFile(filepath.Join(dst, name), []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
		t.Logf("wrote %s", name)
	}
}
