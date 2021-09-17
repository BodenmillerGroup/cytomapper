# On disk: Plots in tab 2 are correctly rendered

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
                <label class="control-label" id="exprs_marker_1-label" for="exprs_marker_1">
                  <span style="color: black">Select marker 1</span>
                </label>
                <div>
                  <select id="exprs_marker_1"><option value="H3" selected>H3</option>
    <option value="CD99">CD99</option>
    <option value="PIN">PIN</option>
    <option value="CD8a">CD8a</option>
    <option value="CDH">CDH</option></select>
                  <script type="application/json" data-for="exprs_marker_1" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
                </div>
              </div>
            </div>
            <div class="col-sm-6">
              <div class="form-group shiny-input-container">
                <label class="control-label" id="exprs_marker_2-label" for="exprs_marker_2">
                  <span style="color: black">Select marker 2</span>
                </label>
                <div>
                  <select id="exprs_marker_2"><option value="H3">H3</option>
    <option value="CD99">CD99</option>
    <option value="PIN">PIN</option>
    <option value="CD8a">CD8a</option>
    <option value="CDH">CDH</option>
    <option value="" selected></option></select>
                  <script type="application/json" data-for="exprs_marker_2">{"plugins":["selectize-plugin-a11y"]}</script>
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
                <label class="control-label" id="exprs_marker_1-label" for="exprs_marker_1">
                  <span style="color: black">Select marker 1</span>
                </label>
                <div>
                  <select id="exprs_marker_1"><option value="H3" selected>H3</option>
    <option value="CD99">CD99</option>
    <option value="PIN">PIN</option>
    <option value="CD8a">CD8a</option>
    <option value="CDH">CDH</option></select>
                  <script type="application/json" data-for="exprs_marker_1" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
                </div>
              </div>
              <div class="form-group shiny-input-container">
                <label class="control-label" id="contrast_marker_1-label" for="contrast_marker_1">
                  <span style="color: black; padding-top: 0px">Contrast marker 1</span>
                </label>
                <input id="contrast_marker_1" type="number" class="form-control" value="1"/>
              </div>
            </div>
            <div class="col-sm-6">
              <div class="form-group shiny-input-container">
                <label class="control-label" id="exprs_marker_2-label" for="exprs_marker_2">
                  <span style="color: black">Select marker 2</span>
                </label>
                <div>
                  <select id="exprs_marker_2"><option value="H3">H3</option>
    <option value="CD99">CD99</option>
    <option value="PIN">PIN</option>
    <option value="CD8a">CD8a</option>
    <option value="CDH">CDH</option>
    <option value="" selected></option></select>
                  <script type="application/json" data-for="exprs_marker_2">{"plugins":["selectize-plugin-a11y"]}</script>
                </div>
              </div>
              <div class="form-group shiny-input-container">
                <label class="control-label" id="contrast_marker_2-label" for="contrast_marker_2">
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

