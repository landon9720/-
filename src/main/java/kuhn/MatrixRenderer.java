package kuhn;

import com.googlecode.lanterna.TerminalPosition;
import com.googlecode.lanterna.TerminalSize;
import com.googlecode.lanterna.TextColor;
import com.googlecode.lanterna.gui2.InteractableRenderer;
import com.googlecode.lanterna.gui2.TextGUIGraphics;

import static java.lang.Math.max;

import kuhn.Colors.*;

public class MatrixRenderer implements InteractableRenderer<Matrix> {

    @Override
    public TerminalPosition getCursorLocation(Matrix component) {
        if (component.activeColIndex() - component.colViewOffset() < 0) return null;
        final TerminalPosition origin = getOrigin(component);
        return origin.withRelative(component.activeColIndex() - component.colViewOffset() + 1, component.activeRowIndex() + 1);
    }

    @Override
    public TerminalSize getPreferredSize(Matrix component) {
        return new TerminalSize(300, 3 + component.rows().size());
    }

    @Override
    public void drawComponent(TextGUIGraphics graphics, Matrix component) {

        graphics.applyThemeStyle(component.getTheme().getDefaultDefinition().getNormal());

        final TerminalPosition origin = getOrigin(component);
        final int originX = origin.getColumn();
        final int originY = origin.getRow();
        final int colViewOffset = component.colViewOffset();
        final int width = graphics.getSize().getColumns() - 20;
        final int height = component.rows().size();

        // labels and axises
        graphics.setForegroundColor(kuhn.Colors.FG_AXIS());
        graphics.putString(origin.getColumn(), origin.getRow(), "+");
        graphics.setForegroundColor(kuhn.Colors.FG());
        graphics.putString(origin.getColumn() - component.name().length(), origin.getRow(), component.name());
        for (int x = 0; x < width; x++) {
            TextColor bg;
            if ((x + colViewOffset) % component.gridX() == 0) {
                bg = kuhn.Colors.GRID_Y_STRIPE_BG();
            } else {
                bg = kuhn.Colors.BG();
            }
            if (component.showTransportControl() && x + colViewOffset == component.transportLocationScaled()) {
                bg = kuhn.Colors.TRANSPORT_CURSOR_BG();
            }
            graphics.setBackgroundColor(bg);
            graphics.setForegroundColor(kuhn.Colors.FG_AXIS());
            graphics.putString(1 + originX + x, originY, "-");
            graphics.setForegroundColor(kuhn.Colors.FG());
            if (x + colViewOffset < 0) continue;
            if ((x + colViewOffset) % 10 == 0) {
                graphics.putString(1 + originX + x, originY - 2, String.valueOf((x + colViewOffset) % 100 / 10));
            } else {
                graphics.putString(1 + originX + x, originY - 2, " ");
            }
            graphics.putString(1 + originX + x, originY - 1, String.valueOf((x + colViewOffset) % 10));
        }
        for (int y = 0, y0 = 0; y < height; y++) {
            graphics.applyThemeStyle(component.getThemeDefinition().getNormal());
            graphics.putString(0, 1 + origin.getRow() + y0, component.rows().apply(y).name());
            graphics.setForegroundColor(kuhn.Colors.FG_AXIS());
            graphics.putString(origin.getColumn(), 1 + origin.getRow() + y0, "|");
            y0++;
        }

        // cell values
        for (int x = 0; x < width; x++) {
            if (x + colViewOffset < 0) continue;
            for (int y = 0, y0 = 0; y < height; y++) {
                Val val = component.valAt((x + colViewOffset), y);
                if (!val.negative()) graphics.applyThemeStyle(component.getThemeDefinition().getCustom("value"));
                else                 graphics.applyThemeStyle(component.getThemeDefinition().getCustom("value_negative"));
                if ((x + colViewOffset) % component.gridX() == 0) {
                    graphics.setBackgroundColor(kuhn.Colors.GRID_Y_STRIPE_BG());
                }
                graphics.putString(1 + originX + x, 1 + originY + y0, Character.toString(val.value()));
                y0++;
            }
        }
    }

    private TerminalPosition getOrigin(Matrix component) {
        int w = max(component.name().length(), 20);
        return new TerminalPosition(w, 2);
    }

}
