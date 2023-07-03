require "test_helper"

class UpperCaseNameControllerTest < ActionDispatch::IntegrationTest
  test "should get index" do
    get upper_case_name_index_url
    assert_response :success
  end
end
