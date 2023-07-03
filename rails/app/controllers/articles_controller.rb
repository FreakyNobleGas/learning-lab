class ArticlesController < ApplicationController
  def index
    @articles = Article.all
  end
  def show
    # @article is an instance variable, and by default, Rails passes all instance variables to the view
    @article = Article.find(params[:id])
  end
  # Defines the POST action for /articles/new
  def new
    # Instantiates a new Article, but does not save it. This allows the page to be rendered, without validations like create.
    # Renders new.html.erb
    @article = Article.new
  end

  def create
    # By default, the parameters will be the fields of the form defined in articles/new.html.erb
    # Parameters: {"authenticity_token"=>"[FILTERED]", "article"=>{"title"=>"", "text"=>""}, "commit"=>"Save Article"}
    # render plain: params[:article].inspect

    # The class Article is defined in the model.
    @article = Article.new(article_params)

    if @article.save
      redirect_to @article
    else
      render :new, status: :unprocessable_entity
    end
  end

  # Similar to new, where the article is fetched, but no changes are made.
  def edit
    @article = Article.find(params[:id])
  end

  # Refetches the article (if used after edit), runs validations and stores in DB
  def update
    @article = Article.find(params[:id])

    if @article.update(article_params)
      redirect_to @article
    else
      render :edit, status: :unprocessable_entity
    end
  end

  private

  def article_params
    # require and permit are security features to limit the amount of parameters allowed to pass
    # http://weblog.rubyonrails.org/2012/3/21/strong-parameters/
    params.require(:article).permit(:title, :body)
  end
end
