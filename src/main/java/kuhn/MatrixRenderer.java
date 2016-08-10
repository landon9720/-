package kuhn;

import com.googlecode.lanterna.TerminalPosition;
import com.googlecode.lanterna.TerminalSize;
import com.googlecode.lanterna.graphics.ThemeDefinition;
import com.googlecode.lanterna.gui2.InteractableRenderer;
import com.googlecode.lanterna.gui2.TextGUIGraphics;

public class MatrixRenderer implements InteractableRenderer<MatrixComponent> {

    @Override
    public TerminalPosition getCursorLocation(MatrixComponent component) {
        return component.cursorPosition();
    }

    @Override
    public TerminalSize getPreferredSize(MatrixComponent component) {
        return new TerminalSize(component.matrix().width(), component.matrix().height());
    }

    @Override
    public void drawComponent(TextGUIGraphics graphics, MatrixComponent component) {
        ThemeDefinition themeDefinition = component.getThemeDefinition();
        if (component.isFocused()) {
            graphics.applyThemeStyle(themeDefinition.getActive());
        }
        else {
            graphics.applyThemeStyle(themeDefinition.getInsensitive());
        }
        for (int x = 0; x < component.matrix().width(); x++) {
            for (int y = 0; y < component.matrix().height(); y++) {
                Character value = component.matrix().apply(x, y);
                graphics.putString(x, y, value.toString());
            }
        }
    }
}
