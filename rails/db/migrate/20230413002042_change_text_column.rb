class ChangeTextColumn < ActiveRecord::Migration[7.0]
  def change
    rename_column(:articles, :text, :body)
  end
end
