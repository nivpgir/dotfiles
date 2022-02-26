//:-s
use eframe::{egui, epi};

#[derive(Default)]
pub struct App {
    label: String,
    text: String,
}

impl epi::App for App {
    fn name(&self) -> &str {
        &self.label
    }

    fn setup( &mut self,
	       _ctx: &egui::Context,
	       _frame: &epi::Frame,
	       _storage: Option<&dyn epi::Storage>){
    }

    fn update(&mut self, ctx: &egui::Context, frame: &epi::Frame) {
	egui::Area::new("main area").show(ctx, |ui|{
	    if ui.input().key_released(egui::Key::Escape) {
		frame.quit();
	    }
	    egui::ScrollArea::vertical()
		.auto_shrink([false, false])
		.show(ui, |ui| {
		    ui.label(&self.text)
		});
	});
    }
}


let mut app = App::default();
match io::stdin().read_to_string(&mut app.text) {
    Ok(l) => print!("{}", l),
    Err(err) => panic!("ERROR, Failed to read from stdin because: {}", err),
};

let native_options = eframe::NativeOptions{
    transparent: true,
    decorated: false,
    ..Default::default()
};

eframe::run_native(Box::new(app), native_options);
