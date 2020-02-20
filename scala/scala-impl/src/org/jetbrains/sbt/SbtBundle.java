package org.jetbrains.sbt;

import com.intellij.DynamicBundle;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.PropertyKey;

public class SbtBundle extends DynamicBundle {
    @NonNls
    private static final String BUNDLE = "messages.SbtBundle";

    private static final SbtBundle INSTANCE = new SbtBundle();

    private SbtBundle() {
        super(BUNDLE);
    }

    public static String message(@NotNull @PropertyKey(resourceBundle = BUNDLE) String key, @NotNull Object... params) {
        return INSTANCE.getMessage(key, params);
    }
}
