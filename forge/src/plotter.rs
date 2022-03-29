use crate::{
    executor::{CHEATCODE_ADDRESS, HARDHAT_CONSOLE_ADDRESS},
    trace::{CallTraceArena, RawOrDecodedCall, TraceKind},
};
use comfy_table::{modifiers::UTF8_ROUND_CORNERS, presets::UTF8_FULL, *};
use ethers::types::U256;
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap, fmt::Display, cmp::Ordering};

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct PlotReport {
    pub report_for: Vec<String>,
    pub graphs: BTreeMap<String, Vec<GraphData>>,
}

struct GraphData {
    contract: String,
    func: String,
    args: Vec<String>,
    output: String,
}



impl GasReport {
    pub fn new(report_for: Vec<String>) -> Self {
        Self { report_for, ..Default::default() }
    }

    pub fn analyze(&mut self, traces: &[(TraceKind, CallTraceArena)]) {
        let report_for_all = self.report_for.is_empty() || self.report_for.iter().any(|s| s == "*");
        traces.iter().for_each(|(_, trace)| {
            self.analyze_trace(trace, report_for_all);
        });
    }

    fn analyze_trace(&mut self, trace: &CallTraceArena, report_for_all: bool) {
        self.analyze_node(0, trace, report_for_all);
    }

    fn analyze_node(&mut self, node_index: usize, arena: &CallTraceArena, report_for_all: bool) {
        let node = &arena.arena[node_index];
        let trace = &node.trace;

        if trace.address == CHEATCODE_ADDRESS || trace.address == HARDHAT_CONSOLE_ADDRESS {
            return
        }

        if let Some(name) = &trace.contract {
            let report_for = self.report_for.iter().any(|s| s == name);
            if report_for || report_for_all {

                match &trace.data {
                    // TODO: More robust test contract filtering
                    RawOrDecodedCall::Decoded(func, args) if !func.starts_with("plot") => {
                        let data = GraphData {
                            contract: name,
                            func,
                            args,
                            output: trace.output.clone(),
                        }
                        let name = format!("{}_{}", name, func);
                        let graph = self.graphs.entry(name).or_insert_with(Vec::new());
                        graph.push(data);
                    }
                    _ => (),
                }
            }
        }

        node.children.iter().for_each(|index| {
            self.analyze_node(*index, arena, report_for_all);
        });
    }

    #[must_use]
    pub fn finalize(mut self) {
        
        self.graphs.iter().for_each(|key, data| {
            let data = data.filter(|graph_data| 
                graph_data.args.len() > 0 &&
                graph_data.args[0].parse::<U256>().is_ok() &&
                graph_data.output.parse::<U256>().is_ok()
            )
            .map(|graph_data| {
                (graph_data.args[0].parse::<U256>().unwrap(), graph_data.output.parse::<U256>().unwrap())
            })
            .collect::<Vec<(U256, U256)>>()
            .sort_by(|a, b| {
                if a.0 > b.0 {
                    Ordering::Greater
                } else if a.0 < b.0 {
                    Ordering::Less
                } else {
                    Ordering::Equal
                }
            });
            let _ = draw_chart(0f64, 255f64, 0f64, 510f64, &data, key);
        });

        // self.contracts.iter_mut().for_each(|(_, contract)| {
        //     contract.functions.iter_mut().for_each(|(_, func)| {
        //         func.calls.sort();
        //         func.min = func.calls.first().cloned().unwrap_or_default();
        //         func.max = func.calls.last().cloned().unwrap_or_default();
        //         func.mean =
        //             func.calls.iter().fold(U256::zero(), |acc, x| acc + x) / func.calls.len();

        //         let len = func.calls.len();
        //         func.median = if len > 0 {
        //             if len % 2 == 0 {
        //                 (func.calls[len / 2 - 1] + func.calls[len / 2]) / 2
        //             } else {
        //                 func.calls[len / 2]
        //             }
        //         } else {
        //             0.into()
        //         };
        //     });
        // });
        // self
    }
}

impl Display for PlotReport {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        for (name, contract) in self.contracts.iter() {
            let mut table = Table::new();
            table.load_preset(UTF8_FULL).apply_modifier(UTF8_ROUND_CORNERS);
            table.set_header(vec![Cell::new(format!("{} contract", name))
                .add_attribute(Attribute::Bold)
                .fg(Color::Green)]);
            table.add_row(vec![
                Cell::new("Deployment Cost").add_attribute(Attribute::Bold).fg(Color::Cyan),
                Cell::new("Deployment Size").add_attribute(Attribute::Bold).fg(Color::Cyan),
            ]);
            table.add_row(vec![contract.gas.to_string(), contract.size.to_string()]);

            table.add_row(vec![
                Cell::new("Function Name").add_attribute(Attribute::Bold).fg(Color::Magenta),
                Cell::new("min").add_attribute(Attribute::Bold).fg(Color::Green),
                Cell::new("avg").add_attribute(Attribute::Bold).fg(Color::Yellow),
                Cell::new("median").add_attribute(Attribute::Bold).fg(Color::Yellow),
                Cell::new("max").add_attribute(Attribute::Bold).fg(Color::Red),
                Cell::new("# calls").add_attribute(Attribute::Bold),
            ]);
            contract.functions.iter().for_each(|(fname, function)| {
                table.add_row(vec![
                    Cell::new(fname.to_string()).add_attribute(Attribute::Bold),
                    Cell::new(function.min.to_string()).fg(Color::Green),
                    Cell::new(function.mean.to_string()).fg(Color::Yellow),
                    Cell::new(function.median.to_string()).fg(Color::Yellow),
                    Cell::new(function.max.to_string()).fg(Color::Red),
                    Cell::new(function.calls.len().to_string()),
                ]);
            });
            writeln!(f, "{}", table)?
        }
        Ok(())
    }
}

fn draw_chart(min_x: f64, max_x: f64, min_y: f64, max_y: f64, data: &[(U256, U256)], name: &str) -> Result<(), Box<dyn std::error::Error>> {
    let name = format!("{}.png", name);
    let root = BitMapBackend::new(name, (640, 480)).into_drawing_area();
    root.fill(&WHITE)?;

    let mut chart = ChartBuilder::on(&root)
        .caption("y=x^2", ("sans-serif", 50).into_font())
        .margin(5u32)
        .x_label_area_size(30u32)
        .y_label_area_size(30u32)
        .build_cartesian_2d(min_x..max_x, min_y..max_y)?;

    chart.configure_mesh().draw()?;

    chart
        .draw_series(LineSeries::new(
            data.iter().map(|(x, y)| (*x, *y)),
            &RED,
        ))?
        .label("y = x*2");
        //.legend(|(x, y)| PathElement::new(vec![(x, y), (x + 20, y)], &RED));

    chart
        .configure_series_labels()
        .background_style(&WHITE.mix(0.8))
        .border_style(&BLACK)
        .draw()?;
    Ok(())
}