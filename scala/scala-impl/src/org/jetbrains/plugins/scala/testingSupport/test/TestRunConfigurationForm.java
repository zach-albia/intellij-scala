package org.jetbrains.plugins.scala.testingSupport.test;

import com.intellij.application.options.ModulesComboBox;
import com.intellij.execution.ExecutionBundle;
import com.intellij.execution.configuration.BrowseModuleValueActionListener;
import com.intellij.execution.configuration.EnvironmentVariablesComponent;
import com.intellij.execution.ui.ClassBrowser;
import com.intellij.execution.ui.ConfigurationModuleSelector;
import com.intellij.execution.ui.DefaultJreSelector;
import com.intellij.execution.ui.JrePathEditor;
import com.intellij.ide.util.ClassFilter;
import com.intellij.ide.util.PackageChooserDialog;
import com.intellij.openapi.fileChooser.FileChooserDescriptor;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiClass;
import com.intellij.psi.PsiPackage;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.ui.AnActionButtonRunnable;
import com.intellij.ui.RawCommandLineEditor;
import com.intellij.ui.ToolbarDecorator;
import com.intellij.ui.table.JBTable;
import com.intellij.uiDesigner.core.GridConstraints;
import com.intellij.uiDesigner.core.GridLayoutManager;
import com.intellij.uiDesigner.core.Spacer;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.plugins.scala.ScalaBundle;
import org.jetbrains.plugins.scala.lang.psi.ScalaPsiUtil;
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiManager;
import org.jetbrains.plugins.scala.testingSupport.test.testdata.RegexpTestData;
import org.jetbrains.plugins.scala.testingSupport.test.testdata.SingleTestData;
import org.jetbrains.plugins.scala.testingSupport.test.testdata.TestConfigurationData;
import org.jetbrains.sbt.settings.SbtSettings;
import scala.Option;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import java.awt.*;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.ResourceBundle;

@SuppressWarnings(value = "unchecked")
public class TestRunConfigurationForm {
  private JPanel myPanel;
  private TextFieldWithBrowseButton testClassTextField;
  private RawCommandLineEditor VMParamsTextField;
  private RawCommandLineEditor testOptionsTextField;
  private TextFieldWithBrowseButton testPackageTextField;
  private JLabel testClassLabel;
  private JLabel testPackageLabel;
  private ModulesComboBox moduleComboBox;
  private TextFieldWithBrowseButton workingDirectoryField;
  private JPanel searchForTestsPanel;
  private final ConfigurationModuleSelector myModuleSelector;
  private final java.util.List<String> suitePaths;

  private JComboBox<SearchForTest> searchForTestsComboBox;

  private JComboBox<TestKind> kindComboBox;

  private JTextArea testNameTextField;
  private JLabel testNameLabel;
  private JCheckBox myShowProgressMessagesCheckBox;
  private EnvironmentVariablesComponent environmentVariables;
  private JBTable regexpTable;
  private JLabel regexpLabel;
  private JScrollPane testNameScrollPane;
  private JPanel regexpPanel;
  private JCheckBox useSbtCheckBox;
  private JCheckBox useUiWithSbt;
  private JrePathEditor jreSelector;

  private void createUIComponents() {
    regexpTable = new JBTable();
    final DefaultTableModel model = (DefaultTableModel) regexpTable.getModel();
    model.addColumn(ScalaBundle.message("test.run.config.for.class.pattern"));
    model.addColumn(ScalaBundle.message("test.run.config.test.pattern"));

    AnActionButtonRunnable addAction = anActionButton -> {
      CellEditor editor = regexpTable.getCellEditor();
      int rowAdd = regexpTable.getSelectedRow() + 1;
      if (editor != null) editor.stopCellEditing();
      model.insertRow(rowAdd, new Object[]{"", ""});
      if (rowAdd == 0) regexpTable.requestFocus();
      regexpTable.setRowSelectionInterval(rowAdd, rowAdd);
      regexpTable.setColumnSelectionInterval(0, 0);
    };

    AnActionButtonRunnable removeAction = anActionButton -> {
      int row = regexpTable.getSelectedRow();
      if (row != -1) {
        CellEditor editor = regexpTable.getCellEditor();
        if (editor != null) editor.stopCellEditing();
        model.removeRow(row);
        if (row > 0) {
          regexpTable.setRowSelectionInterval(row - 1, row - 1);
          regexpTable.setColumnSelectionInterval(0, 0);
        }
      }
    };

    regexpPanel = ToolbarDecorator.createDecorator(regexpTable)
            .setAddAction(addAction)
            .setRemoveAction(removeAction)
            .createPanel();
  }

  public enum SearchForTest {
    IN_WHOLE_PROJECT("In whole project"),
    IN_SINGLE_MODULE("In single module"),
    ACCROSS_MODULE_DEPENDENCIES("Across module dependencies");

    // TODO: proper i18, currently this value is used for both UI-displaying and for persisting
    private final String value;

    SearchForTest(@NonNls String representation) {
      this.value = representation;
    }

    @Override
    public String toString() {
      return value;
    }

    public static SearchForTest parse(String str) {
      if (IN_SINGLE_MODULE.value.equals(str)) return IN_SINGLE_MODULE;
      else if (IN_WHOLE_PROJECT.value.equals(str)) return IN_WHOLE_PROJECT;
      else return ACCROSS_MODULE_DEPENDENCIES;
    }
  }

  public enum TestKind {
    ALL_IN_PACKAGE("All in package"),
    CLAZZ("Class"),
    TEST_NAME("Test name"),
    REGEXP("Regular expression");

    // TODO: same as for SearchForTest
    private final String value;

    TestKind(@NonNls String value) {
      this.value = value;
    }

    @Override
    public String toString() {
      return value;
    }

    public static TestKind parse(String s) {
      if (ALL_IN_PACKAGE.value.equals(s)) return ALL_IN_PACKAGE;
      else if (CLAZZ.value.equals(s)) return CLAZZ;
      else if (TEST_NAME.value.equals(s)) return TEST_NAME;
      else if (REGEXP.value.equals(s)) return REGEXP;
      else return null;
    }
  }

  public TestRunConfigurationForm(final Project project, final AbstractTestRunConfiguration configuration) {
    $$$setupUI$$$();
    jreSelector.setDefaultJreSelector(DefaultJreSelector.fromModuleDependencies(moduleComboBox, false));
    myModuleSelector = new ConfigurationModuleSelector(project, moduleComboBox);
    myModuleSelector.reset(configuration);
    moduleComboBox.setEnabled(true);
    addClassChooser(ScalaBundle.message("test.run.config.choose.test.class"), testClassTextField, project);
    addFileChooser(ScalaBundle.message("test.run.config.choose.working.directory"), workingDirectoryField, project);
    final TestConfigurationData testConfigurationData = configuration.testConfigurationData();
    workingDirectoryField.setText(testConfigurationData.getWorkingDirectory());

    addPackageChooser(testPackageTextField, project);

    for (SearchForTest searchForTest : SearchForTest.values()) {
      searchForTestsComboBox.addItem(searchForTest);
    }

    searchForTestsComboBox.setSelectedItem(testConfigurationData.getSearchTest());

    searchForTestsComboBox.addItemListener(e -> setupModuleComboBox());

    myShowProgressMessagesCheckBox.setSelected(testConfigurationData.getShowProgressMessages());

    for (TestKind testKind : TestKind.values()) {
      kindComboBox.addItem(testKind);
    }

    switch (testConfigurationData.getKind()) {
      case ALL_IN_PACKAGE:
        setPackageEnabled();
        break;
      case CLAZZ:
        setClassEnabled();
        break;
      case TEST_NAME:
        setTestNameEnabled();
        break;
      case REGEXP:
        setRegexpEnabled();
    }
    useSbtCheckBox.addItemListener(e -> {
      testConfigurationData.setUseSbt(useSbtCheckBox.isSelected());
      useUiWithSbt.setEnabled(useSbtCheckBox.isSelected());
    });
    boolean hasSbt = hasSbt(configuration.getProject());
    setSbtVisible(hasSbt);
    setSbtUiVisible(hasSbt && configuration.allowsSbtUiRun());
    useUiWithSbt.setEnabled(useSbtCheckBox.isSelected());

    kindComboBox.addItemListener(e -> {
      moduleComboBox.setEnabled(true);
      switch ((TestKind) e.getItem()) {
        case ALL_IN_PACKAGE:
          setPackageEnabled();
          setupModuleComboBox();
          break;
        case CLAZZ:
          setClassEnabled();
          break;
        case TEST_NAME:
          setTestNameEnabled();
          break;
        case REGEXP:
          setRegexpEnabled();
      }
    });

    suitePaths = configuration.javaSuitePaths();
    environmentVariables.setEnvs(testConfigurationData.envs());
  }

  private void setupModuleComboBox() {
    Object selectedItem = searchForTestsComboBox.getSelectedItem();
    if (selectedItem == null) return;
    switch ((SearchForTest) selectedItem) {
      case IN_WHOLE_PROJECT:
        moduleComboBox.setEnabled(false);
        break;
      case IN_SINGLE_MODULE:
        moduleComboBox.setEnabled(true);
        break;
      case ACCROSS_MODULE_DEPENDENCIES:
        moduleComboBox.setEnabled(true);
        break;
    }
  }

  private void setSbtVisible(boolean visible) {
    useSbtCheckBox.setVisible(visible);
  }

  private void setSbtUiVisible(boolean visible) {
    useUiWithSbt.setVisible(visible);
  }

  private void setPackageVisible(boolean visible) {
    testPackageLabel.setVisible(visible);
    testPackageTextField.setVisible(visible);
    searchForTestsPanel.setVisible(visible);
  }

  private void setClassVisible(boolean visible) {
    testClassLabel.setVisible(visible);
    testClassTextField.setVisible(visible);
  }

  private void setTestNameVisible(boolean visible) {
    testNameLabel.setVisible(visible);
    testNameTextField.setVisible(visible);
    testClassLabel.setVisible(visible);
    testClassTextField.setVisible(visible);
    testNameScrollPane.setVisible(visible);
  }

  private void setRegexpVisible(boolean visible) {
    regexpLabel.setVisible(visible);
    regexpTable.setVisible(visible);
    regexpPanel.setVisible(visible);
  }

  private void disableAll() {
    setPackageVisible(false);
    setClassVisible(false);
    setTestNameVisible(false);
    setRegexpVisible(false);
  }

  private void setPackageEnabled() {
    disableAll();
    setPackageVisible(true);
    kindComboBox.setSelectedItem(TestKind.ALL_IN_PACKAGE);
  }

  private void setClassEnabled() {
    disableAll();
    setClassVisible(true);
    kindComboBox.setSelectedItem(TestKind.CLAZZ);
  }

  private void setTestNameEnabled() {
    disableAll();
    setTestNameVisible(true);
    kindComboBox.setSelectedItem(TestKind.TEST_NAME);
  }

  private void setRegexpEnabled() {
    disableAll();
    setRegexpVisible(true);
    kindComboBox.setSelectedItem(TestKind.REGEXP);
  }

  public void apply(AbstractTestRunConfiguration configuration) {
    setTestClassPath(configuration.getTestClassPath());
    setJavaOptions(configuration.testConfigurationData().javaOptions());
    setTestArgs(configuration.testConfigurationData().getTestArgs());
    setTestPackagePath(configuration.getTestPackagePath());
    switch (configuration.testConfigurationData().getKind()) {
      case ALL_IN_PACKAGE:
        setPackageEnabled();
        break;
      case CLAZZ:
        setClassEnabled();
        break;
      case TEST_NAME:
        SingleTestData singleData = (SingleTestData) configuration.testConfigurationData();
        setTestName(singleData.getTestName());
        setTestNameEnabled();
        break;
      case REGEXP:
        RegexpTestData regexpData = (RegexpTestData) configuration.testConfigurationData();
        setRegexps(regexpData.getClassRegexps(), regexpData.getTestRegexps());
        setRegexpEnabled();
    }
    boolean hasSbt = hasSbt(configuration.getProject());
    setSbtVisible(hasSbt);
    setSbtUiVisible(hasSbt && configuration.allowsSbtUiRun());
    setUseSbt(configuration.testConfigurationData().useSbt());
    setUseUiWithSbt(configuration.testConfigurationData().useUiWithSbt());
    setWorkingDirectory(configuration.testConfigurationData().getWorkingDirectory());
    myModuleSelector.applyTo(configuration);
    searchForTestsComboBox.setSelectedItem(configuration.testConfigurationData().getSearchTest());
    environmentVariables.setEnvs(configuration.testConfigurationData().envs());
    setShowProgressMessages(configuration.testConfigurationData().getShowProgressMessages());
    jreSelector.setPathOrName(configuration.testConfigurationData().getJrePath(), true);
  }

  protected boolean hasSbt(Project project) {
    SbtSettings sbtSettings = SbtSettings.getInstance(project);
    return sbtSettings != null && !sbtSettings.getLinkedProjectsSettings().isEmpty();
  }

  public TestKind getSelectedKind() {
    return (TestKind) kindComboBox.getSelectedItem();
  }

  public SearchForTest getSearchForTest() {
    return (SearchForTest) searchForTestsComboBox.getSelectedItem();
  }

  public String getTestClassPath() {
    return testClassTextField.getText();
  }

  public String getTestArgs() {
    return testOptionsTextField.getText();
  }

  public String getJavaOptions() {
    return VMParamsTextField.getText();
  }

  public String getTestPackagePath() {
    return testPackageTextField.getText();
  }

  public String getWorkingDirectory() {
    return workingDirectoryField.getText();
  }

  public Map<String, String> getEnvironmentVariables() {
    return environmentVariables.getEnvs();
  }

  public void setTestClassPath(String s) {
    testClassTextField.setText(s);
  }

  public void setTestArgs(String s) {
    testOptionsTextField.setText(s);
  }

  public void setJavaOptions(String s) {
    VMParamsTextField.setText(s);
  }

  public void setTestPackagePath(String s) {
    testPackageTextField.setText(s);
  }

  public void setWorkingDirectory(String s) {
    workingDirectoryField.setText(s);
  }

  public void setUseSbt(boolean b) {
    useSbtCheckBox.setSelected(b);
  }

  public void setUseUiWithSbt(boolean b) {
    useUiWithSbt.setSelected(b);
  }

  public void setRegexps(String[] classRegexps, String[] testRegexps) {
    final DefaultTableModel model = (DefaultTableModel) regexpTable.getModel();
    for (int i = 0; i < Math.max(classRegexps.length, testRegexps.length); i++) {
      String classRegexp = (i < classRegexps.length) ? classRegexps[i] : "";
      String testRegexp = (i < testRegexps.length) ? testRegexps[i] : "";
      model.addRow(new Object[]{classRegexp, testRegexp});
    }
  }

  protected String[] getRegexpTableColumn(int column) {
    final DefaultTableModel model = (DefaultTableModel) regexpTable.getModel();
    String[] result = new String[model.getRowCount()];
    for (int i = 0; i < model.getRowCount(); i++) {
      result[i] = model.getValueAt(i, column).toString();
    }
    return result;
  }

  public String[] getClassRegexps() {
    return getRegexpTableColumn(0);
  }

  public String[] getTestRegexps() {
    return getRegexpTableColumn(1);
  }

  public boolean getUseSbt() {
    return useSbtCheckBox.isSelected();
  }

  public boolean getUseUiWithSbt() {
    return useUiWithSbt.isSelected();
  }

  public String getTestName() {
    return testNameTextField.getText();
  }

  public void setTestName(String s) {
    testNameTextField.setText(s);
  }

  public boolean getShowProgressMessages() {
    return myShowProgressMessagesCheckBox.isSelected();
  }

  public void setShowProgressMessages(boolean b) {
    myShowProgressMessagesCheckBox.setSelected(b);
  }

  public JPanel getPanel() {
    return myPanel;
  }

  private void addClassChooser(final String title,
                               final TextFieldWithBrowseButton textField,
                               final Project project) {
    ClassBrowser browser = new ClassBrowser(project, title) {
      protected ClassFilter.ClassFilterWithScope getFilter() {
        return new ClassFilter.ClassFilterWithScope() {
          public GlobalSearchScope getScope() {
            Module module = getModule();
            if (module != null) return GlobalSearchScope.moduleWithDependenciesAndLibrariesScope(module);
            return GlobalSearchScope.allScope(project);
          }

          public boolean isAccepted(PsiClass aClass) {
            if (!getScope().accept(aClass.getContainingFile().getVirtualFile())) return false;
            for (String suitePath : suitePaths) {
              PsiClass[] classes = ScalaPsiManager.instance(project).getCachedClasses(getScope(), suitePath);
              for (PsiClass psiClass : classes) {
                if (ScalaPsiUtil.isInheritorDeep(aClass, psiClass)) return true;
              }
            }
            return false;
          }
        };
      }

      protected PsiClass findClass(String className) {
        Option<PsiClass> cachedClass = ScalaPsiManager.instance(project).getCachedClass(GlobalSearchScope.allScope(project), className);
        if (cachedClass.isEmpty()) {
          return null;
        } else {
          return cachedClass.get();
        }
      }
    };

    browser.setField(textField);
  }

  private FileChooserDescriptor addFileChooser(final String title,
                                               final TextFieldWithBrowseButton textField,
                                               final Project project) {
    final FileChooserDescriptor fileChooserDescriptor = new FileChooserDescriptor(false, true, false, false, false, false) {
      @Override
      public boolean isFileVisible(VirtualFile file, boolean showHiddenFiles) {
        return super.isFileVisible(file, showHiddenFiles) && file.isDirectory();
      }
    };
    fileChooserDescriptor.setTitle(title);
    textField.addBrowseFolderListener(title, null, project, fileChooserDescriptor);
    return fileChooserDescriptor;
  }

  private void addPackageChooser(final TextFieldWithBrowseButton textField, final Project project) {
    PackageChooserActionListener browser = new PackageChooserActionListener(project);
    browser.setField(textField);
  }

  //todo: copied from JUnitConfigurable
  private static class PackageChooserActionListener extends BrowseModuleValueActionListener {
    public PackageChooserActionListener(final Project project) {
      super(project);
    }

    @Override
    protected String showDialog() {
      final PackageChooserDialog dialog = new PackageChooserDialog(ExecutionBundle.message("choose.package.dialog.title"), getProject());
      dialog.show();
      final PsiPackage aPackage = dialog.getSelectedPackage();
      return aPackage != null ? aPackage.getQualifiedName() : null;
    }
  }

  public Module getModule() {
    return myModuleSelector.getModule();
  }

  public String getJrePath() {
    return jreSelector.getJrePathOrName();
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
    myPanel.setLayout(new GridLayoutManager(14, 2, new Insets(0, 0, 0, 0), -1, -1));
    final Spacer spacer1 = new Spacer();
    myPanel.add(spacer1, new GridConstraints(13, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_VERTICAL, 1, GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
    final JLabel label1 = new JLabel();
    this.$$$loadLabelText$$$(label1, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.sbt.runner.form.vmParameters"));
    myPanel.add(label1, new GridConstraints(2, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    VMParamsTextField = new RawCommandLineEditor();
    myPanel.add(VMParamsTextField, new GridConstraints(3, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
    final JLabel label2 = new JLabel();
    this.$$$loadLabelText$$$(label2, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.test.options"));
    myPanel.add(label2, new GridConstraints(5, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    testOptionsTextField = new RawCommandLineEditor();
    myPanel.add(testOptionsTextField, new GridConstraints(6, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
    final JPanel panel1 = new JPanel();
    panel1.setLayout(new GridLayoutManager(10, 1, new Insets(0, 0, 0, 0), -1, -1));
    myPanel.add(panel1, new GridConstraints(0, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
    testPackageTextField = new TextFieldWithBrowseButton();
    panel1.add(testPackageTextField, new GridConstraints(8, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
    testPackageLabel = new JLabel();
    this.$$$loadLabelText$$$(testPackageLabel, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.test.package"));
    panel1.add(testPackageLabel, new GridConstraints(7, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    testClassTextField = new TextFieldWithBrowseButton();
    panel1.add(testClassTextField, new GridConstraints(2, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
    testClassLabel = new JLabel();
    this.$$$loadLabelText$$$(testClassLabel, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.test.class"));
    panel1.add(testClassLabel, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    final JPanel panel2 = new JPanel();
    panel2.setLayout(new GridLayoutManager(1, 3, new Insets(0, 0, 0, 0), -1, -1));
    panel1.add(panel2, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
    final JLabel label3 = new JLabel();
    this.$$$loadLabelText$$$(label3, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.test.kind"));
    panel2.add(label3, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    final Spacer spacer2 = new Spacer();
    panel2.add(spacer2, new GridConstraints(0, 2, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, 1, null, null, null, 0, false));
    kindComboBox = new JComboBox();
    final DefaultComboBoxModel defaultComboBoxModel1 = new DefaultComboBoxModel();
    kindComboBox.setModel(defaultComboBoxModel1);
    panel2.add(kindComboBox, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    searchForTestsPanel = new JPanel();
    searchForTestsPanel.setLayout(new GridLayoutManager(1, 3, new Insets(0, 0, 0, 0), -1, -1));
    panel1.add(searchForTestsPanel, new GridConstraints(9, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
    final JLabel label4 = new JLabel();
    this.$$$loadLabelText$$$(label4, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.search.for.tests"));
    searchForTestsPanel.add(label4, new GridConstraints(0, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    final Spacer spacer3 = new Spacer();
    searchForTestsPanel.add(spacer3, new GridConstraints(0, 2, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_WANT_GROW, 1, null, null, null, 0, false));
    searchForTestsComboBox = new JComboBox();
    searchForTestsPanel.add(searchForTestsComboBox, new GridConstraints(0, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    testNameLabel = new JLabel();
    this.$$$loadLabelText$$$(testNameLabel, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.test.name"));
    panel1.add(testNameLabel, new GridConstraints(3, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    testNameScrollPane = new JScrollPane();
    panel1.add(testNameScrollPane, new GridConstraints(4, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
    testNameTextField = new JTextArea();
    testNameTextField.setLineWrap(true);
    testNameScrollPane.setViewportView(testNameTextField);
    regexpLabel = new JLabel();
    this.$$$loadLabelText$$$(regexpLabel, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.regular.expressions"));
    panel1.add(regexpLabel, new GridConstraints(5, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    panel1.add(regexpPanel, new GridConstraints(6, 0, 1, 1, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_BOTH, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_WANT_GROW, null, null, null, 0, false));
    final JLabel label5 = new JLabel();
    this.$$$loadLabelText$$$(label5, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.use.classpath.of.module"));
    myPanel.add(label5, new GridConstraints(9, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    moduleComboBox = new ModulesComboBox();
    myPanel.add(moduleComboBox, new GridConstraints(10, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    final JLabel label6 = new JLabel();
    this.$$$loadLabelText$$$(label6, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.working.directory"));
    myPanel.add(label6, new GridConstraints(7, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_FIXED, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    workingDirectoryField = new TextFieldWithBrowseButton();
    myPanel.add(workingDirectoryField, new GridConstraints(8, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
    myShowProgressMessagesCheckBox = new JCheckBox();
    this.$$$loadButtonText$$$(myShowProgressMessagesCheckBox, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.print.information.messages.to.console"));
    myPanel.add(myShowProgressMessagesCheckBox, new GridConstraints(12, 0, 1, 2, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    environmentVariables = new EnvironmentVariablesComponent();
    myPanel.add(environmentVariables, new GridConstraints(4, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
    useSbtCheckBox = new JCheckBox();
    this.$$$loadButtonText$$$(useSbtCheckBox, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.use.sbt"));
    myPanel.add(useSbtCheckBox, new GridConstraints(1, 0, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 1, false));
    useUiWithSbt = new JCheckBox();
    this.$$$loadButtonText$$$(useUiWithSbt, this.$$$getMessageFromBundle$$$("org/jetbrains/plugins/scala/ScalaBundle", "test.run.config.use.ui.with.sbt"));
    myPanel.add(useUiWithSbt, new GridConstraints(1, 1, 1, 1, GridConstraints.ANCHOR_WEST, GridConstraints.FILL_NONE, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_FIXED, null, null, null, 0, false));
    jreSelector = new JrePathEditor();
    myPanel.add(jreSelector, new GridConstraints(11, 0, 1, 2, GridConstraints.ANCHOR_CENTER, GridConstraints.FILL_HORIZONTAL, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, GridConstraints.SIZEPOLICY_CAN_SHRINK | GridConstraints.SIZEPOLICY_CAN_GROW, null, null, null, 0, false));
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

}
