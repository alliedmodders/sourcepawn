<?php
// vim: set ts=4 sw=4 tw=99 et:
    $Title = ( empty( $HeaderTitle ) ? '' : ( htmlspecialchars( $HeaderTitle ) . ' · ' ) ) . $Project . ' Scripting API Reference';
    
    if( $RenderLayout ):
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta http-equiv="X-UA-Compatible" content="IE=edge">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    
    <title><?php echo $Title; ?></title>
    
    <link rel="stylesheet" href="//cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.2/css/bootstrap.min.css">
    <link rel="stylesheet" href="<?php echo $BaseURL; ?>style.css">
</head>
<body data-baseurl="<?php echo $BaseURL; ?>">
    <div class="sidebar">
        <div class="header-link">
            <a href="<?php echo $BaseURL; ?>"><?php echo $Project; ?> API</a>
        </div>
        
        <input class="form-control typeahead" type="text" placeholder="Search">
        <span id="js-search-clear" style="display:none">&times;</span>
        
        <noscript>
            <style>
                .typeahead {
                    display: none;
                }
                
                .bg-primary {
                    padding: 10px;
                    text-align: center;
                }
            </style>
            
            <p class="bg-primary">Search requires javascript to work</p>
        </noscript>
        
        <div id="js-functions"></div>
        <div id="js-functions-default">
            <?php require __DIR__ . '/sidebar.php'; ?>
        </div>
    </div>
    
    <div class="container-fluid">
        <div class="row">
            <div class="col-lg-12" id="pjax-container">
<?php else: ?>
<title><?php echo $Title; ?></title>
<?php endif; ?>
