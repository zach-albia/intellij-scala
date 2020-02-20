package org.jetbrains.plugins.scala.settings;

import com.intellij.ide.projectView.ProjectView;
import com.intellij.ide.projectView.impl.AbstractProjectViewPane;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.updateSettings.impl.UpdateChecker;
import com.intellij.openapi.updateSettings.impl.UpdateSettings;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.ui.EnumComboBoxModel;
import com.intellij.ui.SimpleListCellRenderer;
import com.intellij.ui.TitledSeparator;
import com.intellij.uiDesigner.core.GridConstraints;
import com.intellij.uiDesigner.core.GridLayoutManager;
import com.intellij.uiDesigner.core.Spacer;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.plugins.scala.ScalaFileType;
import org.jetbrains.plugins.scala.codeInspection.bundled.BundledInspectionsUiTableModel;
import org.jetbrains.plugins.scala.compiler.ScalaCompileServerSettings;
import org.jetbrains.plugins.scala.components.InvalidRepoException;
import org.jetbrains.plugins.scala.components.ScalaPluginUpdater;
import org.jetbrains.plugins.scala.components.libextensions.ui.LibExtensionsSettingsPanelWrapper;
import org.jetbrains.plugins.scala.settings.uiControls.ScalaUiWithDependency;
import org.jetbrains.plugins.scala.worksheet.interactive.WorksheetAutoRunner$;

import javax.swing.*;
import java.awt.*;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.ResourceBundle;

import static org.jetbrains.plugins.scala.settings.uiControls.ScalaUiWithDependency.ComponentWithSettings;
import static org.jetbrains.plugins.scala.settings.uiControls.ScalaUiWithDependency.EP_NAME;

/**
 * User: Alexander Podkhalyuzin
 * Date: 30.07.2008
 */
// TODO: cleanup
//  1) split this panel into multiple per-tab
//  2) worksheet slider indicator SCL-16341
//  3) use bundles SCL-16963
@SuppressWarnings(value = "unchecked")
public class ScalaProjectSettingsPanel {

    private static final String LAST_SELECTED_TAB_INDEX = "scala_project_settings_configurable.last_selected_tab_index";

    private final LibExtensionsSettingsPanelWrapper extensionsPanel;

    private JPanel myPanel;
    private JCheckBox searchAllSymbolsIncludeCheckBox;
    private JCheckBox enableConversionOnCopyCheckBox;
    private JCheckBox donTShowDialogCheckBox;
    private JCheckBox showImplicitConversionsInCheckBox;
    private JCheckBox myResolveToAllClassesCheckBox;
    private JCheckBox showArgumentsToByNameParametersCheckBox;
    private JCheckBox includeBlockExpressionsExpressionsCheckBox;
    private JCheckBox includeLiteralsCheckBox;
    private JCheckBox treatDocCommentAsBlockComment;
    private JCheckBox myDisableLanguageInjection;
    private JCheckBox useScalaClassesPriorityCheckBox;
    private JComboBox collectionHighlightingChooser;
    private JPanel injectionJPanel;
    private JSpinner outputSpinner;
    private JSpinner implicitParametersSearchDepthSpinner;
    private JCheckBox myDontCacheCompound;
    private JCheckBox runWorksheetInTheCheckBox;
    private JTextArea myBasePackages;
    private JCheckBox showTypeInfoOnCheckBox;
    private JSpinner delaySpinner;
    private JComboBox updateChannel;
    private JCheckBox myAotCompletion;
    private JCheckBox useEclipseCompatibilityModeCheckBox;
    private JTextField scalaTestDefaultSuperClass;
    private JCheckBox treatScalaScratchFilesCheckBox;
    private JSlider autoRunDelaySlider;
    private JCheckBox customScalatestSyntaxHighlightingCheckbox;
    private JPanel librariesPanel;
    private JButton updateNowButton;
    private JCheckBox addOverrideToImplementCheckBox;
    private JCheckBox myProjectViewHighlighting;
    private JComboBox scalaMetaMode;
    private JCheckBox metaTrimBodies;
    private JComboBox scTypeSelectionCombobox;
    private JComboBox trailingCommasComboBox;
    private JCheckBox collapseWorksheetFoldByCheckBox;
    private JCheckBox showNotFoundImplicitArgumentsCheckBox;
    private JCheckBox myGroupPackageObjectWithPackage;
    private JComboBox ivy2IndexingModeCBB;
    private Project myProject;

    private JTabbedPane tabbedPane;

    private final List<ComponentWithSettings> extraSettings = new ArrayList<>();

    public ScalaProjectSettingsPanel(Project project) {
        myProject = project;
        $$$setupUI$$$();
        outputSpinner.setModel(new SpinnerNumberModel(35, 1, null, 1));
        updateChannel.setModel(new EnumComboBoxModel(ScalaApplicationSettings.pluginBranch.class));
        scalaMetaMode.setModel(new EnumComboBoxModel(ScalaProjectSettings.ScalaMetaMode.class));
        ivy2IndexingModeCBB.setModel(new EnumComboBoxModel(ScalaProjectSettings.Ivy2IndexingMode.class));
        updateNowButton.addActionListener(e -> {
            try {
                ScalaPluginUpdater.doUpdatePluginHosts((ScalaApplicationSettings.pluginBranch) updateChannel.getModel().getSelectedItem());
                UpdateChecker.updateAndShowResult(myProject, UpdateSettings.getInstance());
            } catch (InvalidRepoException ex) {
                Messages.showErrorDialog(ex.getMessage(), "Invalid Update Channel");
            }
        });


        extensionsPanel = new LibExtensionsSettingsPanelWrapper((JPanel) librariesPanel.getParent(), project);
        extensionsPanel.build();

        for (ScalaUiWithDependency uiWithDependency : EP_NAME.getExtensionList()) {
            extraSettings.add(uiWithDependency.createComponent(injectionJPanel));
        }

        trailingCommasComboBox.setModel(new EnumComboBoxModel<>(ScalaProjectSettings.TrailingCommasMode.class));

        scTypeSelectionCombobox.setModel(new EnumComboBoxModel(ScalaProjectSettings.ScFileMode.class));
        scTypeSelectionCombobox.setRenderer(new SimpleListCellRenderer() {
            @Override
            public void customize(JList jList, Object o, int i, boolean b, boolean b1) {
                if (!(o instanceof ScalaProjectSettings.ScFileMode)) return;

                switch ((ScalaProjectSettings.ScFileMode) o) {
                    case Auto:
                        setText("Ammonite in test sources, otherwise Worksheet");
                        break;
                    case Ammonite:
                        setText("Always Ammonite");
                        break;
                    case Worksheet:
                        setText("Always Worksheet");
                }
            }
        });

        autoRunDelaySlider.setMaximum(WorksheetAutoRunner$.MODULE$.RUN_DELAY_MS_MAXIMUM());
        autoRunDelaySlider.setMinimum(WorksheetAutoRunner$.MODULE$.RUN_DELAY_MS_MINIMUM());

        setSettings();

        initSelectedTab();
    }

    @NotNull
    protected FileType getFileType() {
        return ScalaFileType.INSTANCE;
    }

    public void apply() throws ConfigurationException {
        if (!isModified()) return;

        final ScalaProjectSettings scalaProjectSettings = ScalaProjectSettings.getInstance(myProject);
        final ScalaCompileServerSettings compileServerSettings = ScalaCompileServerSettings.getInstance();

        compileServerSettings.SHOW_TYPE_TOOLTIP_ON_MOUSE_HOVER = showTypeInfoOnCheckBox.isSelected();
        compileServerSettings.SHOW_TYPE_TOOLTIP_DELAY = (Integer) delaySpinner.getValue();

        try {
            ScalaPluginUpdater.doUpdatePluginHostsAndCheck((ScalaApplicationSettings.pluginBranch) updateChannel.getModel().getSelectedItem());
        } catch (InvalidRepoException e) {
            throw new ConfigurationException(e.getMessage());
        }

        scalaProjectSettings.setBasePackages(getBasePackages());
        scalaProjectSettings.setScalaTestDefaultSuperClass(scalaTestDefaultSuperClass.getText());
        scalaProjectSettings.setImplicitParametersSearchDepth((Integer) implicitParametersSearchDepthSpinner.getValue());
        scalaProjectSettings.setOutputLimit((Integer) outputSpinner.getValue());
        scalaProjectSettings.setInProcessMode(runWorksheetInTheCheckBox.isSelected());
        scalaProjectSettings.setWorksheetFoldCollapsedByDefault(collapseWorksheetFoldByCheckBox.isSelected());
        scalaProjectSettings.setUseEclipseCompatibility(useEclipseCompatibilityModeCheckBox.isSelected());
        scalaProjectSettings.setTreatScratchFilesAsWorksheet(treatScalaScratchFilesCheckBox.isSelected());

        scalaProjectSettings.setSearchAllSymbols(searchAllSymbolsIncludeCheckBox.isSelected());
        scalaProjectSettings.setEnableJavaToScalaConversion(enableConversionOnCopyCheckBox.isSelected());
        scalaProjectSettings.setAddOverrideToImplementInConverter(addOverrideToImplementCheckBox.isSelected());
        scalaProjectSettings.setDontShowConversionDialog(donTShowDialogCheckBox.isSelected());
        scalaProjectSettings.setTreatDocCommentAsBlockComment(treatDocCommentAsBlockComment.isSelected());

        scalaProjectSettings.setShowImplisitConversions(showImplicitConversionsInCheckBox.isSelected());
        scalaProjectSettings.setShowNotFoundImplicitArguments(showNotFoundImplicitArgumentsCheckBox.isSelected());
        scalaProjectSettings.setShowArgumentsToByNameParams(showArgumentsToByNameParametersCheckBox.isSelected());
        if (scalaProjectSettings.isCustomScalatestSyntaxHighlighting() != customScalatestSyntaxHighlightingCheckbox.isSelected()) {
            //settings have changed. but actual highlighting will only change on next pass of highlighting visitors
            new ScalaTestHighlightingDialog().show();
        }
        scalaProjectSettings.setCustomScalatestSyntaxHighlighting(customScalatestSyntaxHighlightingCheckbox.isSelected());
        scalaProjectSettings.setIncludeBlockExpressions(includeBlockExpressionsExpressionsCheckBox.isSelected());
        scalaProjectSettings.setIncludeLiterals(includeLiteralsCheckBox.isSelected());

        scalaProjectSettings.setIgnorePerformance(myResolveToAllClassesCheckBox.isSelected());
        scalaProjectSettings.setDisableLangInjection(myDisableLanguageInjection.isSelected());
        scalaProjectSettings.setDontCacheCompoundTypes(myDontCacheCompound.isSelected());
        scalaProjectSettings.setAotCOmpletion(myAotCompletion.isSelected());
        scalaProjectSettings.setScalaPriority(useScalaClassesPriorityCheckBox.isSelected());
        scalaProjectSettings.setCollectionTypeHighlightingLevel(collectionHighlightingChooser.getSelectedIndex());
        scalaProjectSettings.setAutoRunDelay(getWorksheetDelay());

        if (scalaProjectSettings.isProjectViewHighlighting() && !myProjectViewHighlighting.isSelected()) {
            ProblemSolverUtils.clearProblemsIn(myProject);
        }
        scalaProjectSettings.setProjectViewHighlighting(myProjectViewHighlighting.isSelected());
        if (scalaProjectSettings.isGroupPackageObjectWithPackage() != myGroupPackageObjectWithPackage.isSelected()) {
            AbstractProjectViewPane pane = ProjectView.getInstance(myProject).getCurrentProjectViewPane();
            if (pane != null) {
                pane.updateFromRoot(true);
            }
        }
        scalaProjectSettings.setGroupPackageObjectWithPackage(myGroupPackageObjectWithPackage.isSelected());

        scalaProjectSettings.setScalaMetaMode((ScalaProjectSettings.ScalaMetaMode) scalaMetaMode.getModel().getSelectedItem());
        scalaProjectSettings.setMetaTrimMethodBodies(metaTrimBodies.isSelected());

        scalaProjectSettings.setIvy2IndexingMode((ScalaProjectSettings.Ivy2IndexingMode) ivy2IndexingModeCBB.getModel().getSelectedItem());

        scalaProjectSettings.setScFileMode(ScalaProjectSettings.ScFileMode.valueOf(scTypeSelectionCombobox.getSelectedItem().toString()));
        scalaProjectSettings.setTrailingCommasMode(ScalaProjectSettings.TrailingCommasMode.valueOf(trailingCommasComboBox.getSelectedItem().toString()));

        scalaProjectSettings.setEnableLibraryExtensions(extensionsPanel.enabledCB().isSelected());

        extraSettings.forEach(s -> s.saveSettings(scalaProjectSettings));
    }

    private List<String> getBasePackages() {
        String[] parts = myBasePackages.getText().split("\\n|,|;");
        List<String> result = new ArrayList<String>();
        for (String part : parts) {
            String name = part.trim();
            if (!name.isEmpty()) result.add(name);
        }
        return result;
    }

    private void setBasePackages(List<String> packages) {
        String s = StringUtil.join(packages, "\n");
        myBasePackages.setText(s);
    }

    @SuppressWarnings({"ConstantConditions", "RedundantIfStatement"})
    public boolean isModified() {

        final ScalaProjectSettings scalaProjectSettings = ScalaProjectSettings.getInstance(myProject);
        final ScalaCompileServerSettings compileServerSettings = ScalaCompileServerSettings.getInstance();

        if (compileServerSettings.SHOW_TYPE_TOOLTIP_ON_MOUSE_HOVER != showTypeInfoOnCheckBox.isSelected()) return true;
        if (compileServerSettings.SHOW_TYPE_TOOLTIP_DELAY != (Integer) delaySpinner.getValue()) return true;

        if (!ScalaPluginUpdater.getScalaPluginBranch().equals(updateChannel.getModel().getSelectedItem())) return true;

        if (!scalaProjectSettings.getBasePackages().equals(
                getBasePackages())) return true;
        if (!scalaProjectSettings.getScalaTestDefaultSuperClass().equals(
                scalaTestDefaultSuperClass.getText())) return true;
        if (scalaProjectSettings.isShowImplisitConversions() !=
                showImplicitConversionsInCheckBox.isSelected()) return true;
        if (scalaProjectSettings.isShowNotFoundImplicitArguments() !=
                showNotFoundImplicitArgumentsCheckBox.isSelected()) return true;
        if (scalaProjectSettings.isShowArgumentsToByNameParams() !=
                showArgumentsToByNameParametersCheckBox.isSelected()) return true;
        if (scalaProjectSettings.isCustomScalatestSyntaxHighlighting() !=
                customScalatestSyntaxHighlightingCheckbox.isSelected()) return true;
        if (scalaProjectSettings.isIncludeBlockExpressions() !=
                includeBlockExpressionsExpressionsCheckBox.isSelected()) return true;
        if (scalaProjectSettings.isIncludeLiterals() !=
                includeLiteralsCheckBox.isSelected()) return true;

        if (scalaProjectSettings.getImplicitParametersSearchDepth() !=
                (Integer) implicitParametersSearchDepthSpinner.getValue()) return true;
        if (scalaProjectSettings.getOutputLimit() !=
                (Integer) outputSpinner.getValue()) return true;
        if (scalaProjectSettings.isInProcessMode() !=
                runWorksheetInTheCheckBox.isSelected()) return true;
        if (scalaProjectSettings.isWorksheetFoldCollapsedByDefault() !=
                collapseWorksheetFoldByCheckBox.isSelected()) return true;
        if (scalaProjectSettings.isUseEclipseCompatibility() != useEclipseCompatibilityModeCheckBox.isSelected())
            return true;
        if (scalaProjectSettings.isTreatScratchFilesAsWorksheet() != treatScalaScratchFilesCheckBox.isSelected())
            return true;

        if (scalaProjectSettings.isSearchAllSymbols() !=
                searchAllSymbolsIncludeCheckBox.isSelected()) return true;
        if (scalaProjectSettings.isEnableJavaToScalaConversion() !=
                enableConversionOnCopyCheckBox.isSelected()) return true;
        if (scalaProjectSettings.isAddOverrideToImplementInConverter() !=
                addOverrideToImplementCheckBox.isSelected()) return true;
        if (scalaProjectSettings.isDontShowConversionDialog() !=
                donTShowDialogCheckBox.isSelected()) return true;
        if (scalaProjectSettings.isTreatDocCommentAsBlockComment() !=
                treatDocCommentAsBlockComment.isSelected()) return true;

        if (scalaProjectSettings.isIgnorePerformance() != myResolveToAllClassesCheckBox.isSelected())
            return true;

        if (scalaProjectSettings.isDisableLangInjection() != myDisableLanguageInjection.isSelected())
            return true;


        if (scalaProjectSettings.isDontCacheCompoundTypes() != myDontCacheCompound.isSelected()) return true;

        if (scalaProjectSettings.isAotCompletion() != myAotCompletion.isSelected())
            return true;

        if (scalaProjectSettings.isProjectViewHighlighting() != myProjectViewHighlighting.isSelected())
            return true;

        if (scalaProjectSettings.isGroupPackageObjectWithPackage() != myGroupPackageObjectWithPackage.isSelected())
            return true;

        if (scalaProjectSettings.isScalaPriority() != useScalaClassesPriorityCheckBox.isSelected())
            return true;

        if (scalaProjectSettings.getCollectionTypeHighlightingLevel() !=
                collectionHighlightingChooser.getSelectedIndex()) return true;

        if (scalaProjectSettings.getAutoRunDelay() != getWorksheetDelay()) return true;

        for (ComponentWithSettings setting : extraSettings) {
            if (setting.isModified(scalaProjectSettings)) return true;
        }

        if (!scalaProjectSettings.getScalaMetaMode().equals(scalaMetaMode.getModel().getSelectedItem())) return true;
        if (scalaProjectSettings.isMetaTrimMethodBodies() != metaTrimBodies.isSelected()) return true;

        if (!scalaProjectSettings.getIvy2IndexingMode().equals(ivy2IndexingModeCBB.getModel().getSelectedItem()))
            return true;

        if (scalaProjectSettings.getScFileMode() != scTypeSelectionCombobox.getSelectedItem()) return true;
        if (scalaProjectSettings.getTrailingCommasMode() != trailingCommasComboBox.getSelectedItem()) return true;

        if (scalaProjectSettings.isEnableLibraryExtensions() != extensionsPanel.enabledCB().isSelected()) return true;

        return false;
    }

    public JComponent getPanel() {
        return myPanel;
    }

    void dispose() {
        int tabIdx = tabbedPane.getSelectedIndex();
        PropertiesComponent.getInstance(myProject).setValue(LAST_SELECTED_TAB_INDEX, tabIdx, -1);
    }

    private void initSelectedTab() {
        int tabIdx = PropertiesComponent.getInstance(myProject).getInt(LAST_SELECTED_TAB_INDEX, -1);
        setSelectedTabIndex(tabIdx);
    }

    private void setSelectedTabIndex(int index) {
        if (0 <= index && index < tabbedPane.getTabCount()) {
            tabbedPane.setSelectedIndex(index);
        }
    }

    protected void resetImpl() {
        setSettings();
    }

    private void setSettings() {
        final ScalaProjectSettings scalaProjectSettings = ScalaProjectSettings.getInstance(myProject);
        final ScalaCompileServerSettings compileServerSettings = ScalaCompileServerSettings.getInstance();

        setValue(showTypeInfoOnCheckBox, compileServerSettings.SHOW_TYPE_TOOLTIP_ON_MOUSE_HOVER);
        setValue(delaySpinner, compileServerSettings.SHOW_TYPE_TOOLTIP_DELAY);

        updateChannel.getModel().setSelectedItem(ScalaPluginUpdater.getScalaPluginBranch());

        setBasePackages(scalaProjectSettings.getBasePackages());
        setValue(scalaTestDefaultSuperClass, scalaProjectSettings.getScalaTestDefaultSuperClass());
        setValue(implicitParametersSearchDepthSpinner, scalaProjectSettings.getImplicitParametersSearchDepth());
        setValue(outputSpinner, scalaProjectSettings.getOutputLimit());
        setValue(runWorksheetInTheCheckBox, scalaProjectSettings.isInProcessMode());
        setValue(collapseWorksheetFoldByCheckBox, scalaProjectSettings.isWorksheetFoldCollapsedByDefault());
        setValue(useEclipseCompatibilityModeCheckBox, scalaProjectSettings.isUseEclipseCompatibility());
        setValue(treatScalaScratchFilesCheckBox, scalaProjectSettings.isTreatScratchFilesAsWorksheet());

        setValue(searchAllSymbolsIncludeCheckBox, scalaProjectSettings.isSearchAllSymbols());
        setValue(enableConversionOnCopyCheckBox, scalaProjectSettings.isEnableJavaToScalaConversion());
        setValue(addOverrideToImplementCheckBox, scalaProjectSettings.isAddOverrideToImplementInConverter());
        setValue(donTShowDialogCheckBox, scalaProjectSettings.isDontShowConversionDialog());
        setValue(treatDocCommentAsBlockComment, scalaProjectSettings.isTreatDocCommentAsBlockComment());

        setValue(showImplicitConversionsInCheckBox, scalaProjectSettings.isShowImplisitConversions());
        setValue(showNotFoundImplicitArgumentsCheckBox, scalaProjectSettings.isShowNotFoundImplicitArguments());
        setValue(showArgumentsToByNameParametersCheckBox, scalaProjectSettings.isShowArgumentsToByNameParams());
        setValue(customScalatestSyntaxHighlightingCheckbox, scalaProjectSettings.isCustomScalatestSyntaxHighlighting());
        setValue(includeBlockExpressionsExpressionsCheckBox, scalaProjectSettings.isIncludeBlockExpressions());
        setValue(includeLiteralsCheckBox, scalaProjectSettings.isIncludeLiterals());

        setValue(myResolveToAllClassesCheckBox, scalaProjectSettings.isIgnorePerformance());

        setValue(myDisableLanguageInjection, scalaProjectSettings.isDisableLangInjection());
        setValue(myDontCacheCompound, scalaProjectSettings.isDontCacheCompoundTypes());
        setValue(myAotCompletion, scalaProjectSettings.isAotCompletion());
        setValue(useScalaClassesPriorityCheckBox, scalaProjectSettings.isScalaPriority());
        collectionHighlightingChooser.setSelectedIndex(scalaProjectSettings.getCollectionTypeHighlightingLevel());
        setWorksheetDelay(scalaProjectSettings.getAutoRunDelay());

        setValue(myProjectViewHighlighting, scalaProjectSettings.isProjectViewHighlighting());
        setValue(myGroupPackageObjectWithPackage, scalaProjectSettings.isGroupPackageObjectWithPackage());

        scTypeSelectionCombobox.setSelectedItem(scalaProjectSettings.getScFileMode());
        trailingCommasComboBox.setSelectedItem(scalaProjectSettings.getTrailingCommasMode());

        for (ComponentWithSettings setting : extraSettings) {
            setting.loadSettings(scalaProjectSettings);
        }

        scalaMetaMode.getModel().setSelectedItem(scalaProjectSettings.getScalaMetaMode());
        setValue(metaTrimBodies, scalaProjectSettings.isMetaTrimMethodBodies());

        ivy2IndexingModeCBB.getModel().setSelectedItem(scalaProjectSettings.getIvy2IndexingMode());

        setValue(extensionsPanel.enabledCB(), scalaProjectSettings.isEnableLibraryExtensions());
    }

    private int getWorksheetDelay() {
        return autoRunDelaySlider.getValue();
    }

    private void setWorksheetDelay(int delay) {
        autoRunDelaySlider.setValue(delay);
    }

    private static void setValue(JSpinner spinner, int value) {
        spinner.setValue(value);
    }

    private static void setValue(final JCheckBox box, final boolean value) {
        box.setSelected(value);
    }

    private static void setValue(final JComboBox box, final int value) {
        box.setSelectedIndex(value);
    }

    private static void setValue(final JTextField field, final String value) {
        field.setText(value);
    }

    private void createUIComponents() {
        injectionJPanel = new JPanel(new GridLayout(1, 1));
        injectionJPanel.setPreferredSize(new Dimension(200, 500));
        injectionJPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 0, 0));
    }

    /**
     * Method generated by IntelliJ IDEA GUI Designer
     * >>> IMPORTANT!! <<<
     * DO NOT edit this method OR call it in your code!
     *
     * @noinspection ALL
     */
    private void $$$setupUI$$$() {
        createUIComponents();
        myPanel = new JPanel();
        myPanel.setLayout(new GridLayoutManager(1, 2, new Insets(0, 0, 0, 0), -1, -1));
        tabbedPane = new JTabbedPane();
        myPanel.add(tabbedPane, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, new Dimension(200, 200), null, 0, false));
        final JPanel panel1 = new JPanel();
        panel1.setLayout(new GridLayoutManager(17, 2, new Insets(9, 9, 0, 0), -1, -1));
        tabbedPane.addTab("Editor", panel1);
        final Spacer spacer1 = new Spacer();
        panel1.add(spacer1, new GridConstraints(16, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_VERTICAL, 1, GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
        showImplicitConversionsInCheckBox = new JCheckBox();
        showImplicitConversionsInCheckBox.setSelected(true);
        showImplicitConversionsInCheckBox.setText("Highlight implicit conversions");
        panel1.add(showImplicitConversionsInCheckBox, new GridConstraints(2, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        showArgumentsToByNameParametersCheckBox = new JCheckBox();
        showArgumentsToByNameParametersCheckBox.setSelected(true);
        showArgumentsToByNameParametersCheckBox.setText("Highlight arguments to by-name parameters");
        panel1.add(showArgumentsToByNameParametersCheckBox, new GridConstraints(4, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        includeBlockExpressionsExpressionsCheckBox = new JCheckBox();
        includeBlockExpressionsExpressionsCheckBox.setSelected(true);
        includeBlockExpressionsExpressionsCheckBox.setText("Include block expressions");
        includeBlockExpressionsExpressionsCheckBox.setToolTipText("Include expressions enclosed in in curly braces");
        panel1.add(includeBlockExpressionsExpressionsCheckBox, new GridConstraints(5, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 1, false));
        includeLiteralsCheckBox = new JCheckBox();
        includeLiteralsCheckBox.setSelected(true);
        includeLiteralsCheckBox.setText("Include literals ");
        includeLiteralsCheckBox.setToolTipText("Include string, number, etc");
        panel1.add(includeLiteralsCheckBox, new GridConstraints(6, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 1, false));
        final TitledSeparator titledSeparator1 = new TitledSeparator();
        titledSeparator1.setText("Highlighting");
        panel1.add(titledSeparator1, new GridConstraints(1, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
        final TitledSeparator titledSeparator2 = new TitledSeparator();
        titledSeparator2.setText("Autocomplete");
        panel1.add(titledSeparator2, new GridConstraints(9, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
        myAotCompletion = new JCheckBox();
        myAotCompletion.setText("Ahead-of-time completion (parameter and variable names)");
        panel1.add(myAotCompletion, new GridConstraints(10, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        useScalaClassesPriorityCheckBox = new JCheckBox();
        useScalaClassesPriorityCheckBox.setSelected(true);
        useScalaClassesPriorityCheckBox.setText("Use Scala classes priority over Java classes");
        panel1.add(useScalaClassesPriorityCheckBox, new GridConstraints(11, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        enableConversionOnCopyCheckBox = new JCheckBox();
        enableConversionOnCopyCheckBox.setSelected(true);
        enableConversionOnCopyCheckBox.setText("Convert Java code to Scala on copy-paste");
        panel1.add(enableConversionOnCopyCheckBox, new GridConstraints(13, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        donTShowDialogCheckBox = new JCheckBox();
        donTShowDialogCheckBox.setText("Don't show dialog on paste and automatically convert to Scala code");
        panel1.add(donTShowDialogCheckBox, new GridConstraints(14, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 1, false));
        final TitledSeparator titledSeparator3 = new TitledSeparator();
        titledSeparator3.setText("Code Conversion");
        panel1.add(titledSeparator3, new GridConstraints(12, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
        final JPanel panel2 = new JPanel();
        panel2.setLayout(new GridLayoutManager(1, 2, new Insets(0, 0, 0, 0), -1, -1));
        panel1.add(panel2, new GridConstraints(0, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
        showTypeInfoOnCheckBox = new JCheckBox();
        showTypeInfoOnCheckBox.setText("Show type info on mouse hover after, ms");
        panel2.add(showTypeInfoOnCheckBox, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        delaySpinner = new JSpinner();
        panel2.add(delaySpinner, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        customScalatestSyntaxHighlightingCheckbox = new JCheckBox();
        customScalatestSyntaxHighlightingCheckbox.setText("Custom scalaTest keywords highlighting");
        panel1.add(customScalatestSyntaxHighlightingCheckbox, new GridConstraints(7, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        addOverrideToImplementCheckBox = new JCheckBox();
        addOverrideToImplementCheckBox.setSelected(true);
        addOverrideToImplementCheckBox.setText("Add override keyword to method implementation");
        panel1.add(addOverrideToImplementCheckBox, new GridConstraints(15, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        showNotFoundImplicitArgumentsCheckBox = new JCheckBox();
        showNotFoundImplicitArgumentsCheckBox.setSelected(true);
        showNotFoundImplicitArgumentsCheckBox.setText("Show hints if no implicit arguments found");
        panel1.add(showNotFoundImplicitArgumentsCheckBox, new GridConstraints(3, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JPanel panel3 = new JPanel();
        panel3.setLayout(new GridLayoutManager(1, 2, new Insets(0, 0, 0, 0), -1, -1));
        panel1.add(panel3, new GridConstraints(8, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
        final JLabel label1 = new JLabel();
        this.$$$loadLabelText$$$(label1, this.$$$getMessageFromBundle$$$("messages/ScalaBundle", "collection.type.highlighting.option"));
        panel3.add(label1, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        collectionHighlightingChooser = new JComboBox();
        final DefaultComboBoxModel defaultComboBoxModel1 = new DefaultComboBoxModel();
        defaultComboBoxModel1.addElement("None");
        defaultComboBoxModel1.addElement("Only non-qualified");
        defaultComboBoxModel1.addElement("All");
        collectionHighlightingChooser.setModel(defaultComboBoxModel1);
        panel3.add(collectionHighlightingChooser, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JPanel panel4 = new JPanel();
        panel4.setLayout(new GridLayoutManager(3, 1, new Insets(9, 9, 0, 0), -1, -1));
        tabbedPane.addTab("Project View", panel4);
        myProjectViewHighlighting = new JCheckBox();
        myProjectViewHighlighting.setText("Highlight nodes with errors");
        myProjectViewHighlighting.setMnemonic('H');
        myProjectViewHighlighting.setDisplayedMnemonicIndex(0);
        panel4.add(myProjectViewHighlighting, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final Spacer spacer2 = new Spacer();
        panel4.add(spacer2, new GridConstraints(2, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_VERTICAL, 1, GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
        myGroupPackageObjectWithPackage = new JCheckBox();
        myGroupPackageObjectWithPackage.setText("Group package object with package");
        myGroupPackageObjectWithPackage.setMnemonic('G');
        myGroupPackageObjectWithPackage.setDisplayedMnemonicIndex(0);
        panel4.add(myGroupPackageObjectWithPackage, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JPanel panel5 = new JPanel();
        panel5.setLayout(new GridLayoutManager(10, 2, new Insets(9, 9, 0, 0), -1, -1));
        tabbedPane.addTab("Performance", panel5);
        final Spacer spacer3 = new Spacer();
        panel5.add(spacer3, new GridConstraints(9, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_VERTICAL, 1, GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
        final JLabel label2 = new JLabel();
        label2.setText("Implicit parameters search depth (-1 for none):");
        panel5.add(label2, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        implicitParametersSearchDepthSpinner = new JSpinner();
        panel5.add(implicitParametersSearchDepthSpinner, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, new Dimension(50, -1), null, null, 0, false));
        myResolveToAllClassesCheckBox = new JCheckBox();
        myResolveToAllClassesCheckBox.setText("Resolve to all classes, even in wrong directories (this may cause performance problems)");
        panel5.add(myResolveToAllClassesCheckBox, new GridConstraints(5, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        treatDocCommentAsBlockComment = new JCheckBox();
        treatDocCommentAsBlockComment.setText("Disable parsing of documentation comments. This may improve editor performance for very large files. (SCL-2900)");
        panel5.add(treatDocCommentAsBlockComment, new GridConstraints(6, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        myDisableLanguageInjection = new JCheckBox();
        myDisableLanguageInjection.setText("Disable language injection in Scala files (injected languages may freeze typing with auto popup completion)");
        panel5.add(myDisableLanguageInjection, new GridConstraints(7, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        myDontCacheCompound = new JCheckBox();
        myDontCacheCompound.setText("Don't cache compound types (use it in case of big pauses in GC)");
        panel5.add(myDontCacheCompound, new GridConstraints(8, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        searchAllSymbolsIncludeCheckBox = new JCheckBox();
        searchAllSymbolsIncludeCheckBox.setText("Search all symbols (include locals)");
        panel5.add(searchAllSymbolsIncludeCheckBox, new GridConstraints(4, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JLabel label3 = new JLabel();
        this.$$$loadLabelText$$$(label3, this.$$$getMessageFromBundle$$$("messages/ScalaBundle", "scala.meta.settings.annot212"));
        label3.setToolTipText(this.$$$getMessageFromBundle$$$("messages/ScalaBundle", "scala.meta.settings.annot212Tooltip"));
        panel5.add(label3, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        scalaMetaMode = new JComboBox();
        final DefaultComboBoxModel defaultComboBoxModel2 = new DefaultComboBoxModel();
        defaultComboBoxModel2.addElement("Enabled");
        defaultComboBoxModel2.addElement("Disabled");
        defaultComboBoxModel2.addElement("Manual");
        scalaMetaMode.setModel(defaultComboBoxModel2);
        scalaMetaMode.setToolTipText(this.$$$getMessageFromBundle$$$("messages/ScalaBundle", "scala.meta.settings.modeOptionsTooltip"));
        panel5.add(scalaMetaMode, new GridConstraints(1, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        metaTrimBodies = new JCheckBox();
        this.$$$loadButtonText$$$(metaTrimBodies, this.$$$getMessageFromBundle$$$("messages/ScalaBundle", "scala.meta.settings.trimBodiesCap"));
        metaTrimBodies.setToolTipText(this.$$$getMessageFromBundle$$$("messages/ScalaBundle", "scala.meta.settings.trimBodiesTooltip"));
        panel5.add(metaTrimBodies, new GridConstraints(3, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        ivy2IndexingModeCBB = new JComboBox();
        final DefaultComboBoxModel defaultComboBoxModel3 = new DefaultComboBoxModel();
        defaultComboBoxModel3.addElement("Disabled");
        defaultComboBoxModel3.addElement("Metadata");
        defaultComboBoxModel3.addElement("Classes");
        ivy2IndexingModeCBB.setModel(defaultComboBoxModel3);
        ivy2IndexingModeCBB.setToolTipText(this.$$$getMessageFromBundle$$$("messages/ScalaBundle", "project.settings.sbt.index.ivy2.mode.hint"));
        panel5.add(ivy2IndexingModeCBB, new GridConstraints(2, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JLabel label4 = new JLabel();
        this.$$$loadLabelText$$$(label4, this.$$$getMessageFromBundle$$$("messages/ScalaBundle", "project.settings.sbt.index.ivy2.mode"));
        label4.setToolTipText(this.$$$getMessageFromBundle$$$("messages/ScalaBundle", "project.settings.sbt.index.ivy2.mode.hint"));
        panel5.add(label4, new GridConstraints(2, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JPanel panel6 = new JPanel();
        panel6.setLayout(new GridLayoutManager(8, 6, new Insets(9, 9, 0, 0), -1, -1));
        tabbedPane.addTab("Worksheet", panel6);
        final Spacer spacer4 = new Spacer();
        panel6.add(spacer4, new GridConstraints(7, 0, 1, 6, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_VERTICAL, 1, GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
        runWorksheetInTheCheckBox = new JCheckBox();
        runWorksheetInTheCheckBox.setSelected(true);
        runWorksheetInTheCheckBox.setText("Run worksheet in the compiler process (Plain mode only)");
        panel6.add(runWorksheetInTheCheckBox, new GridConstraints(2, 0, 1, 6, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JLabel label5 = new JLabel();
        label5.setText("Output cutoff limit, lines: ");
        panel6.add(label5, new GridConstraints(1, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        useEclipseCompatibilityModeCheckBox = new JCheckBox();
        useEclipseCompatibilityModeCheckBox.setText("Use \"eclipse compatibility\" mode");
        panel6.add(useEclipseCompatibilityModeCheckBox, new GridConstraints(3, 0, 1, 6, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        treatScalaScratchFilesCheckBox = new JCheckBox();
        treatScalaScratchFilesCheckBox.setText("Treat Scala scratch files as worksheet files");
        panel6.add(treatScalaScratchFilesCheckBox, new GridConstraints(4, 0, 1, 5, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JLabel label6 = new JLabel();
        label6.setText("Delay before auto-run");
        panel6.add(label6, new GridConstraints(6, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        autoRunDelaySlider = new JSlider();
        autoRunDelaySlider.setExtent(0);
        autoRunDelaySlider.setMaximum(3000);
        autoRunDelaySlider.setMinimum(700);
        autoRunDelaySlider.setValue(700);
        panel6.add(autoRunDelaySlider, new GridConstraints(6, 1, 1, 4, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        scTypeSelectionCombobox = new JComboBox();
        panel6.add(scTypeSelectionCombobox, new GridConstraints(0, 1, 1, 4, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JLabel label7 = new JLabel();
        label7.setText("Treat .sc files as:");
        panel6.add(label7, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        collapseWorksheetFoldByCheckBox = new JCheckBox();
        collapseWorksheetFoldByCheckBox.setText("Collapse long output by default");
        panel6.add(collapseWorksheetFoldByCheckBox, new GridConstraints(5, 0, 1, 5, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final Spacer spacer5 = new Spacer();
        panel6.add(spacer5, new GridConstraints(0, 5, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, 1, null, null, null, 0, false));
        outputSpinner = new JSpinner();
        panel6.add(outputSpinner, new GridConstraints(1, 4, 1, 1, GridConstraints.ANCHOR_EAST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, new Dimension(50, -1), null, null, 0, false));
        final JPanel panel7 = new JPanel();
        panel7.setLayout(new GridLayoutManager(2, 2, new Insets(9, 9, 0, 0), -1, -1));
        tabbedPane.addTab("Base packages", panel7);
        final Spacer spacer6 = new Spacer();
        panel7.add(spacer6, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_VERTICAL, 1, GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
        final JScrollPane scrollPane1 = new JScrollPane();
        panel7.add(scrollPane1, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_NORTHWEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
        myBasePackages = new JTextArea();
        myBasePackages.setColumns(50);
        myBasePackages.setRows(10);
        scrollPane1.setViewportView(myBasePackages);
        final Spacer spacer7 = new Spacer();
        panel7.add(spacer7, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, 1, null, null, null, 0, false));
        final JPanel panel8 = new JPanel();
        panel8.setLayout(new GridLayoutManager(4, 3, new Insets(9, 9, 0, 0), -1, -1));
        tabbedPane.addTab("Misc", panel8);
        panel8.add(injectionJPanel, new GridConstraints(2, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
        final JLabel label8 = new JLabel();
        label8.setText("ScalaTest default super class:");
        panel8.add(label8, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final Spacer spacer8 = new Spacer();
        panel8.add(spacer8, new GridConstraints(3, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_VERTICAL, 1, GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
        scalaTestDefaultSuperClass = new JTextField();
        scalaTestDefaultSuperClass.setColumns(25);
        panel8.add(scalaTestDefaultSuperClass, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_FIXED, null, new Dimension(150, -1), null, 0, false));
        final JLabel label9 = new JLabel();
        label9.setText("Trailing commas:");
        panel8.add(label9, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        trailingCommasComboBox = new JComboBox();
        panel8.add(trailingCommasComboBox, new GridConstraints(1, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final Spacer spacer9 = new Spacer();
        panel8.add(spacer9, new GridConstraints(1, 2, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, 1, null, null, null, 0, false));
        final JPanel panel9 = new JPanel();
        panel9.setLayout(new GridLayoutManager(2, 3, new Insets(9, 9, 0, 0), -1, -1));
        tabbedPane.addTab("Updates", panel9);
        final Spacer spacer10 = new Spacer();
        panel9.add(spacer10, new GridConstraints(1, 0, 1, 3, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_VERTICAL, 1, GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
        final JLabel label10 = new JLabel();
        label10.setText("Plugin update channel:");
        panel9.add(label10, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        updateChannel = new JComboBox();
        updateChannel.setEditable(false);
        final DefaultComboBoxModel defaultComboBoxModel4 = new DefaultComboBoxModel();
        defaultComboBoxModel4.addElement("Release");
        defaultComboBoxModel4.addElement("EAP");
        defaultComboBoxModel4.addElement("Nightly");
        updateChannel.setModel(defaultComboBoxModel4);
        panel9.add(updateChannel, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        updateNowButton = new JButton();
        updateNowButton.setText("Check For Updates");
        panel9.add(updateNowButton, new GridConstraints(0, 2, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
        final JPanel panel10 = new JPanel();
        panel10.setLayout(new GridLayoutManager(1, 1, new Insets(9, 9, 0, 0), -1, -1));
        tabbedPane.addTab("Extensions", panel10);
        librariesPanel = new JPanel();
        librariesPanel.setLayout(new GridLayoutManager(1, 1, new Insets(0, 0, 0, 0), -1, -1));
        panel10.add(librariesPanel, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 1, false));
    }

    private static Method $$$cachedGetBundleMethod$$$ = null;

    private String $$$getMessageFromBundle$$$(String path, String key) {
        ResourceBundle bundle;
        try {
            Class<?> thisClass = this.getClass();
            if ($$$cachedGetBundleMethod$$$ == null) {
                Class<?> dynamicBundleClass = thisClass.getClassLoader().loadClass("com.intellij.DynamicBundle");
                $$$cachedGetBundleMethod$$$ = dynamicBundleClass.getMethod("getBundle", String.class, Class.class);
            }
            bundle = (ResourceBundle) $$$cachedGetBundleMethod$$$.invoke(null, path, thisClass);
        } catch (Exception e) {
            bundle = ResourceBundle.getBundle(path);
        }
        return bundle.getString(key);
    }

    /**
     * @noinspection ALL
     */
    private void $$$loadLabelText$$$(JLabel component, String text) {
        StringBuffer result = new StringBuffer();
        boolean haveMnemonic = false;
        char mnemonic = '\0';
        int mnemonicIndex = -1;
        for (int i = 0; i < text.length(); i++) {
            if (text.charAt(i) == '&') {
                i++;
                if (i == text.length()) break;
                if (!haveMnemonic && text.charAt(i) != '&') {
                    haveMnemonic = true;
                    mnemonic = text.charAt(i);
                    mnemonicIndex = result.length();
                }
            }
            result.append(text.charAt(i));
        }
        component.setText(result.toString());
        if (haveMnemonic) {
            component.setDisplayedMnemonic(mnemonic);
            component.setDisplayedMnemonicIndex(mnemonicIndex);
        }
    }

    /**
     * @noinspection ALL
     */
    private void $$$loadButtonText$$$(AbstractButton component, String text) {
        StringBuffer result = new StringBuffer();
        boolean haveMnemonic = false;
        char mnemonic = '\0';
        int mnemonicIndex = -1;
        for (int i = 0; i < text.length(); i++) {
            if (text.charAt(i) == '&') {
                i++;
                if (i == text.length()) break;
                if (!haveMnemonic && text.charAt(i) != '&') {
                    haveMnemonic = true;
                    mnemonic = text.charAt(i);
                    mnemonicIndex = result.length();
                }
            }
            result.append(text.charAt(i));
        }
        component.setText(result.toString());
        if (haveMnemonic) {
            component.setMnemonic(mnemonic);
            component.setDisplayedMnemonicIndex(mnemonicIndex);
        }
    }

    /**
     * @noinspection ALL
     */
    public JComponent $$$getRootComponent$$$() {
        return myPanel;
    }


    private class ScalaTestHighlightingDialog extends DialogWrapper {

        ScalaTestHighlightingDialog() {
            super(myProject);
            init();
        }

        @Nullable
        @Override
        protected JComponent createCenterPanel() {
            JComponent res = new JPanel();
            res.add(new JLabel("Changes in ScalaTest highlighting will be processed correctly only on freshly highlighted files." +
                    "For best experience please restart Intellij IDEA"));
            return res;
        }

        @NotNull
        @Override
        protected Action[] createActions() {
            return new Action[]{getOKAction()};
        }
    }

}
