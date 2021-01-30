# User interface is correctly rendered

    <span class="logo">cytomapper v1.3.3</span>

---

    <nav class="navbar navbar-static-top" role="navigation">
      <span style="display:none;">
        <i class="fa fa-bars"></i>
      </span>
      <a href="#" class="sidebar-toggle" data-toggle="offcanvas" role="button">
        <span class="sr-only">Toggle navigation</span>
      </a>
      <div class="navbar-custom-menu">
        <ul class="nav navbar-nav">
          <li class="dropdown notifications-menu">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown">
              <i class="fa fa-fas fa-download"></i>
            </a>
            <ul class="dropdown-menu">
              <li class="header"></li>
              <li>
                <ul class="menu">
                  <li>
                    <a href="#">
                      <i class="fa fa- text-info"></i>
                      <div class="form-group shiny-input-container">
                        <label class="control-label" for="labelCellsBy">Cell label</label>
                        <input id="labelCellsBy" type="text" class="form-control" value="Cell-Type"/>
                      </div>
                    </a>
                  </li>
                  <li>
                    <a href="#">
                      <i class="fa fa- text-info"></i>
                      <a id="downloadData" class="btn btn-default shiny-download-link " href="" target="_blank" download style="background-color: #3C8DBC; color: white; border-color: #7EA6F8">
                        <i class="fa fa-download"></i>
                        Download selection
                      </a>
                    </a>
                  </li>
                </ul>
              </li>
            </ul>
          </li>
          <li class="dropdown tasks-menu">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown">
              <i class="fa fa-fas fa-question"></i>
            </a>
            <ul class="dropdown-menu">
              <li class="header"></li>
              <li>
                <ul class="menu">
                  <li>
                    <a href="#">
                      <i class="fa fa- text-info"></i>
                      <button id="SessionInfo" type="button" class="btn btn-default action-button" style="background-color: #3C8DBC; color: white; border-color: #3C8DBC">Session Info</button>
                    </a>
                  </li>
                  <li>
                    <a href="#">
                      <i class="fa fa- text-info"></i>
                      <button id="Help" type="button" class="btn btn-default action-button" style="background-color: #3C8DBC; color: white; border-color: #3C8DBC">Help</button>
                    </a>
                  </li>
                </ul>
              </li>
            </ul>
          </li>
        </ul>
      </div>
    </nav>

---

    <section id="sidebarItemExpanded" class="sidebar">
      <ul class="sidebar-menu">
        <li class="treeview">
          <a href="#">
            <i class="fa fa-fas fa-sliders-h"></i>
            <span>General controls</span>
            <i class="fa fa-angle-left pull-right"></i>
          </a>
          <ul class="treeview-menu menu-open" style="display: block;" data-expanded="Generalcontrols">
            <div class="form-group shiny-input-container">
              <label class="control-label" for="plotCount">Select number of plots</label>
              <input class="js-range-slider" id="plotCount" data-min="1" data-max="12" data-from="1" data-step="1" data-grid="true" data-grid-num="5.5" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-data-type="number"/>
            </div>
            <div class="row">
              <div class="col-sm-12" style="padding-left:30px;">
                <p>
                  <strong>Select sample</strong>
                </p>
              </div>
            </div>
            <div class="row">
              <div class="col-sm-2">
                <button id="previous.sample" type="button" class="btn btn-default action-button" style="background-color: transparent; border-color: transparent; color:white; margin-left: 0px;">
                  <i class="fa fa-angle-left fa-2x"></i>
                </button>
              </div>
              <div class="col-sm-8" style="padding-left:0px;padding-right:0px;">
                <div class="form-group shiny-input-container" style="width: 100%;">
                  <label class="control-label shiny-label-null" for="sample"></label>
                  <div>
                    <select id="sample" class="form-control"></select>
                    <script type="application/json" data-for="sample">{"placeholder":"Select a sample","maxItems":1}</script>
                  </div>
                </div>
              </div>
              <div class="col-sm-2" style="padding-left:0px;">
                <button id="next.sample" type="button" class="btn btn-default action-button" style="background-color: transparent; border-color: transparent; color: white; margin-left: 0px; padding-left: 0px;">
                  <i class="fa fa-angle-right fa-2x"></i>
                </button>
              </div>
            </div>
            <div class="form-group shiny-input-container">
              <label class="control-label" for="assay">Select which assay to display</label>
              <div>
                <select id="assay" class="form-control"></select>
                <script type="application/json" data-for="assay">{"placeholder":"Select an assay","maxItems":1}</script>
              </div>
            </div>
          </ul>
        </li>
        <li class="treeview">
          <a href="#">
            <i class="fa fa-far fa-chart-bar"></i>
            <span>Plots</span>
            <i class="fa fa-angle-left pull-right"></i>
          </a>
          <ul class="treeview-menu menu-open" style="display: block;" data-expanded="Plots">
            <div id="AdditionalPlots_sidebar" class="shiny-html-output"></div>
          </ul>
        </li>
        <div id="sidebar" class="sidebarMenuSelectedTabItem" data-value="null"></div>
      </ul>
    </section>

# Sidebar is correctly rendered

    <div class="well" style="background-color: #70C389; border-color: #70C389; padding-bottom: 0px; padding-top: 0px">
      <h3 style="color: black">Plot 1</h3>
      <div class="form-group shiny-input-container">
        <label class="control-label" for="Marker_1">
          <span style="color: black; padding-top: 0px">Select marker 1</span>
        </label>
        <div>
          <select id="Marker_1" class="form-control"></select>
          <script type="application/json" data-for="Marker_1">{"placeholder":"Select marker","maxItems":1}</script>
        </div>
      </div>
      <div class="form-group shiny-input-container">
        <label class="control-label" for="Marker_2">
          <span style="color: black; padding-top: 0px">Select marker 2</span>
        </label>
        <div>
          <select id="Marker_2" class="form-control"></select>
          <script type="application/json" data-for="Marker_2">{"placeholder":"Select marker","maxItems":1}</script>
        </div>
      </div>
    </div>

---

    <div class="well" style="background-color: #70C389; border-color: #70C389; padding-bottom: 0px; padding-top: 0px">
      <h3 style="color: black">Plot 1</h3>
      <div class="form-group shiny-input-container">
        <label class="control-label" for="Marker_1">
          <span style="color: black; padding-top: 0px">Select marker 1</span>
        </label>
        <div>
          <select id="Marker_1" class="form-control"></select>
          <script type="application/json" data-for="Marker_1">{"placeholder":"Select marker","maxItems":1}</script>
        </div>
      </div>
      <div class="form-group shiny-input-container">
        <label class="control-label" for="Marker_2">
          <span style="color: black; padding-top: 0px">Select marker 2</span>
        </label>
        <div>
          <select id="Marker_2" class="form-control"></select>
          <script type="application/json" data-for="Marker_2">{"placeholder":"Select marker","maxItems":1}</script>
        </div>
      </div>
    </div>
    <div class="well" style="background-color: #39BEB4; border-color: #39BEB4; padding-bottom: 0px; padding-top: 0px">
      <h3 style="color: black">Plot 2</h3>
      <div class="form-group shiny-input-container">
        <label class="control-label" for="Marker_3">
          <span style="color: black; padding-top: 0px">Select marker 3</span>
        </label>
        <div>
          <select id="Marker_3" class="form-control"></select>
          <script type="application/json" data-for="Marker_3">{"placeholder":"Select marker","maxItems":1}</script>
        </div>
      </div>
      <div class="form-group shiny-input-container">
        <label class="control-label" for="Marker_4">
          <span style="color: black; padding-top: 0px">Select marker 4</span>
        </label>
        <div>
          <select id="Marker_4" class="form-control"></select>
          <script type="application/json" data-for="Marker_4">{"placeholder":"Select marker","maxItems":1}</script>
        </div>
      </div>
    </div>

---

    <div class="well" style="background-color: #70C389; border-color: #70C389; padding-bottom: 0px; padding-top: 0px">
      <h3 style="color: black">Plot 1</h3>
      <div class="form-group shiny-input-container">
        <label class="control-label" for="Marker_1">
          <span style="color: black; padding-top: 0px">Select marker 1</span>
        </label>
        <div>
          <select id="Marker_1" class="form-control"></select>
          <script type="application/json" data-for="Marker_1">{"placeholder":"Select marker","maxItems":1}</script>
        </div>
      </div>
      <div class="form-group shiny-input-container">
        <label class="control-label" for="Marker_2">
          <span style="color: black; padding-top: 0px">Select marker 2</span>
        </label>
        <div>
          <select id="Marker_2" class="form-control"></select>
          <script type="application/json" data-for="Marker_2">{"placeholder":"Select marker","maxItems":1}</script>
        </div>
      </div>
    </div>
    <div class="well" style="background-color: #39BEB4; border-color: #39BEB4; padding-bottom: 0px; padding-top: 0px">
      <h3 style="color: black">Plot 2</h3>
      <div class="form-group shiny-input-container">
        <label class="control-label" for="Marker_3">
          <span style="color: black; padding-top: 0px">Select marker 3</span>
        </label>
        <div>
          <select id="Marker_3" class="form-control"></select>
          <script type="application/json" data-for="Marker_3">{"placeholder":"Select marker","maxItems":1}</script>
        </div>
      </div>
      <div class="form-group shiny-input-container">
        <label class="control-label" for="Marker_4">
          <span style="color: black; padding-top: 0px">Select marker 4</span>
        </label>
        <div>
          <select id="Marker_4" class="form-control"></select>
          <script type="application/json" data-for="Marker_4">{"placeholder":"Select marker","maxItems":1}</script>
        </div>
      </div>
    </div>
    <div class="well" style="background-color: #3F85A7; border-color: #3F85A7; padding-bottom: 0px; padding-top: 0px">
      <h3 style="color: black">Plot 3</h3>
      <div class="form-group shiny-input-container">
        <label class="control-label" for="Marker_5">
          <span style="color: black; padding-top: 0px">Select marker 5</span>
        </label>
        <div>
          <select id="Marker_5" class="form-control"></select>
          <script type="application/json" data-for="Marker_5">{"placeholder":"Select marker","maxItems":1}</script>
        </div>
      </div>
      <div class="form-group shiny-input-container">
        <label class="control-label" for="Marker_6">
          <span style="color: black; padding-top: 0px">Select marker 6</span>
        </label>
        <div>
          <select id="Marker_6" class="form-control"></select>
          <script type="application/json" data-for="Marker_6">{"placeholder":"Select marker","maxItems":1}</script>
        </div>
      </div>
    </div>

# Plots in tab 1 are correctly rendered

    <div class="row">
      <div class="col-sm-4">
        <div class="box box-primary">
          <div class="box-header">
            <h3 class="box-title">Plot 1</h3>
          </div>
          <div class="box-body">
            <div id="scatter1" class="shiny-plot-output" style="width: 100% ; height: 400px" data-brush-id="plot_brush1" data-brush-fill="#70C389" data-brush-stroke="#70C389" data-brush-opacity="0.25" data-brush-delay="300" data-brush-delay-type="debounce" data-brush-clip="TRUE" data-brush-direction="xy" data-brush-reset-on-new="FALSE"></div>
            <pre id="info1" class="shiny-text-output noplaceholder"></pre>
          </div>
        </div>
      </div>
    </div>

---

    <div class="row">
      <div class="col-sm-4">
        <div class="box box-primary">
          <div class="box-header">
            <h3 class="box-title">Plot 1</h3>
          </div>
          <div class="box-body">
            <div id="scatter1" class="shiny-plot-output" style="width: 100% ; height: 400px" data-brush-id="plot_brush1" data-brush-fill="#70C389" data-brush-stroke="#70C389" data-brush-opacity="0.25" data-brush-delay="300" data-brush-delay-type="debounce" data-brush-clip="TRUE" data-brush-direction="xy" data-brush-reset-on-new="FALSE"></div>
            <pre id="info1" class="shiny-text-output noplaceholder"></pre>
          </div>
        </div>
      </div>
      <div class="col-sm-4">
        <div class="box box-primary">
          <div class="box-header">
            <h3 class="box-title">Plot 2</h3>
          </div>
          <div class="box-body">
            <div id="scatter2" class="shiny-plot-output" style="width: 100% ; height: 400px" data-brush-id="plot_brush2" data-brush-fill="#39BEB4" data-brush-stroke="#39BEB4" data-brush-opacity="0.25" data-brush-delay="300" data-brush-delay-type="debounce" data-brush-clip="TRUE" data-brush-direction="xy" data-brush-reset-on-new="FALSE"></div>
            <pre id="info2" class="shiny-text-output noplaceholder"></pre>
          </div>
        </div>
      </div>
    </div>

---

    <div class="row">
      <div class="col-sm-4">
        <div class="box box-primary">
          <div class="box-header">
            <h3 class="box-title">Plot 1</h3>
          </div>
          <div class="box-body">
            <div id="scatter1" class="shiny-plot-output" style="width: 100% ; height: 400px" data-brush-id="plot_brush1" data-brush-fill="#70C389" data-brush-stroke="#70C389" data-brush-opacity="0.25" data-brush-delay="300" data-brush-delay-type="debounce" data-brush-clip="TRUE" data-brush-direction="xy" data-brush-reset-on-new="FALSE"></div>
            <pre id="info1" class="shiny-text-output noplaceholder"></pre>
          </div>
        </div>
      </div>
      <div class="col-sm-4">
        <div class="box box-primary">
          <div class="box-header">
            <h3 class="box-title">Plot 2</h3>
          </div>
          <div class="box-body">
            <div id="scatter2" class="shiny-plot-output" style="width: 100% ; height: 400px" data-brush-id="plot_brush2" data-brush-fill="#39BEB4" data-brush-stroke="#39BEB4" data-brush-opacity="0.25" data-brush-delay="300" data-brush-delay-type="debounce" data-brush-clip="TRUE" data-brush-direction="xy" data-brush-reset-on-new="FALSE"></div>
            <pre id="info2" class="shiny-text-output noplaceholder"></pre>
          </div>
        </div>
      </div>
      <div class="col-sm-4">
        <div class="box box-primary">
          <div class="box-header">
            <h3 class="box-title">Plot 3</h3>
          </div>
          <div class="box-body">
            <div id="scatter3" class="shiny-plot-output" style="width: 100% ; height: 400px" data-brush-id="plot_brush3" data-brush-fill="#3F85A7" data-brush-stroke="#3F85A7" data-brush-opacity="0.25" data-brush-delay="300" data-brush-delay-type="debounce" data-brush-clip="TRUE" data-brush-direction="xy" data-brush-reset-on-new="FALSE"></div>
            <pre id="info3" class="shiny-text-output noplaceholder"></pre>
          </div>
        </div>
      </div>
    </div>

# Plots in tab 2 are correctly rendered

    <div class="row">
      <div class="col-sm-6">
        <div class="box box-primary" style="height: 550px">
          <div class="box-header">
            <h3 class="box-title">Expression</h3>
          </div>
          <div class="box-body">
            <div class="col-sm-12">
              <button id="resetMarkers" type="button" class="btn btn-default action-button" style="background-color: #46EC46; color: black;">Reset markers</button>
            </div>
            <div class="col-sm-6">
              <div class="form-group shiny-input-container">
                <label class="control-label" for="exprs_marker_1">
                  <span style="color: black">Select marker 1</span>
                </label>
                <div>
                  <select id="exprs_marker_1"><option value="H3" selected>H3</option>
    <option value="CD99">CD99</option>
    <option value="PIN">PIN</option>
    <option value="CD8a">CD8a</option>
    <option value="CDH">CDH</option></select>
                  <script type="application/json" data-for="exprs_marker_1" data-nonempty="">{}</script>
                </div>
              </div>
            </div>
            <div class="col-sm-6">
              <div class="form-group shiny-input-container">
                <label class="control-label" for="exprs_marker_2">
                  <span style="color: black">Select marker 2</span>
                </label>
                <div>
                  <select id="exprs_marker_2"><option value="H3">H3</option>
    <option value="CD99">CD99</option>
    <option value="PIN">PIN</option>
    <option value="CD8a">CD8a</option>
    <option value="CDH">CDH</option>
    <option value="" selected></option></select>
                  <script type="application/json" data-for="exprs_marker_2">{}</script>
                </div>
              </div>
            </div>
            <div class="col-sm-12">
              <div id="image_expression" style="width:100%; height:300px; " class="svgPanZoom html-widget html-widget-output"></div>
            </div>
          </div>
        </div>
      </div>
      <div class="col-sm-6">
        <div class="box box-primary" style="height: 550px">
          <div class="box-header">
            <h3 class="box-title">Selection</h3>
          </div>
          <div class="box-body" id="selection">
            <div class="col-sm-12">
              <div id="image_selection" style="width:100%; height:400px; " class="svgPanZoom html-widget html-widget-output"></div>
            </div>
          </div>
        </div>
      </div>
    </div>

---

    <div class="row">
      <div class="col-sm-6">
        <div class="box box-primary" style="height: 550px">
          <div class="box-header">
            <h3 class="box-title">Expression</h3>
          </div>
          <div class="box-body">
            <div class="col-sm-12">
              <button id="resetMarkers" type="button" class="btn btn-default action-button" style="background-color: #46EC46; color: black;">Reset markers</button>
            </div>
            <div class="col-sm-6">
              <div class="form-group shiny-input-container">
                <label class="control-label" for="exprs_marker_1">
                  <span style="color: black">Select marker 1</span>
                </label>
                <div>
                  <select id="exprs_marker_1"><option value="H3" selected>H3</option>
    <option value="CD99">CD99</option>
    <option value="PIN">PIN</option>
    <option value="CD8a">CD8a</option>
    <option value="CDH">CDH</option></select>
                  <script type="application/json" data-for="exprs_marker_1" data-nonempty="">{}</script>
                </div>
              </div>
              <div class="form-group shiny-input-container">
                <label class="control-label" for="contrast_marker_1">
                  <span style="color: black; padding-top: 0px">Contrast marker 1</span>
                </label>
                <input id="contrast_marker_1" type="number" class="form-control" value="1"/>
              </div>
            </div>
            <div class="col-sm-6">
              <div class="form-group shiny-input-container">
                <label class="control-label" for="exprs_marker_2">
                  <span style="color: black">Select marker 2</span>
                </label>
                <div>
                  <select id="exprs_marker_2"><option value="H3">H3</option>
    <option value="CD99">CD99</option>
    <option value="PIN">PIN</option>
    <option value="CD8a">CD8a</option>
    <option value="CDH">CDH</option>
    <option value="" selected></option></select>
                  <script type="application/json" data-for="exprs_marker_2">{}</script>
                </div>
              </div>
              <div class="form-group shiny-input-container">
                <label class="control-label" for="contrast_marker_2">
                  <span style="color: black; padding-top: 0px">Contrast marker 2</span>
                </label>
                <input id="contrast_marker_2" type="number" class="form-control" value="1"/>
              </div>
            </div>
            <div class="col-sm-12">
              <div id="image_expression" style="width:100%; height:300px; " class="svgPanZoom html-widget html-widget-output"></div>
            </div>
          </div>
        </div>
      </div>
      <div class="col-sm-6">
        <div class="box box-primary" style="height: 550px">
          <div class="box-header">
            <h3 class="box-title">Selection</h3>
          </div>
          <div class="box-body" id="selection">
            <div class="col-sm-12">
              <div id="image_selection" style="width:100%; height:400px; " class="svgPanZoom html-widget html-widget-output"></div>
            </div>
          </div>
        </div>
      </div>
    </div>

